from modgrammar import *
import scope 
from llvmlite import ir
import llvmlite.binding as llvm

i8 = ir.IntType(8)

class Type(object):
    def __init__(self, name=None, llvm=None):
        self._name = name
        self._llvm_type = i8

class SquidGrammar(Grammar):
    def declaration_pass(self, ns):
        ns = self.on_declaration_pass(ns)

        for node in self.find_all(SquidGrammar):
            node.declaration_pass(ns)

    def on_declaration_pass(self, ns):
        return ns

    def get_type(self):
        return self._type

    def check_types(self, context):
        self.on_build_types(context)

        for node in self.find_all(SquidGrammar):
            node.check_types(context)

        self._type = self.on_check_types(context)
        return self._type

    def on_build_types(self, context):
        pass

    def on_check_types(self, context):
        return None

class SquidBinExpr(SquidGrammar):
    def count_rhs(self):
        return len(self.elements[1].elements)

    def get_rhs(self, i):
        return self.elements[1][i].elements[1]

    def get_lhs(self):
        return self.elements[0]

    def get_op(self):
        return self.elements[1][0].elements[0]

    def on_build_types(self, context):
        print("building binexpr types")
        pass

    def on_check_types(self, context):
        t1 = self.get_lhs().get_type()

        for i in range(self.count_rhs()):
            t2 = self.get_rhs(i).get_type()
            if t1 != t2:
                raise TypeError("types don't match: " + t1._name + " and " + t2._name)

        return t1


grammar_whitespace_mode = 'optional'

class NumberLiteral(SquidGrammar):
    grammar = (OPTIONAL('-'), WORD('0-9'), OPTIONAL('.', WORD('0-9')))

    def on_check_types(self, context):
        return context.lookup("i8")

    def value(self):
        return ir.Constant(i8, int(self.string));

class NilLiteral(SquidGrammar):
    grammar = (L('('), L(')'))

    def on_check_types(self, context):
        return None

class HexLiteral(SquidGrammar):
    grammar = (L('0x') | L('0X'), WORD('A-Fa-f0-9'))

class BoolLiteral(SquidGrammar):
    grammar = (L('true') | L('false'))

    def on_check_types(self, context):
        return context.lookup("bool")

    def value(self):
        if self.string == "true":
            return ir.Constant(ir.IntType(1), 1);
        else:
            return ir.Constant(ir.IntType(1), 0);

class StringLiteral(SquidGrammar):
    grammar = (L('\"'), ANY_EXCEPT('\"'), L('\"'))

class UnitTupleLiteral(SquidGrammar):
    grammar = (L('('), REF('Expr'), L(','), L(')'))
    grammar_collapse=True

class NaryTupleLiteral(SquidGrammar):
    grammar = (L('('), LIST_OF(REF('Expr'), min=2), L(')'))
    grammar_collapse=True

class TupleLiteral(SquidGrammar):
    grammar = (NaryTupleLiteral | UnitTupleLiteral)

class ArgsTupleLiteral(SquidGrammar):
    grammar = (L('('), LIST_OF(REF('Expr')), L(')'))

class VectorLiteral(SquidGrammar):
    grammar = (L('['), LIST_OF(REF('Expr')), L(']'))

class MapLiteral(SquidGrammar):
    grammar = (L('{'), LIST_OF(REF('Expr'), L(':'), REF('Expr')), L('}'))

class AggregateLiteral(SquidGrammar):
    grammar = (TupleLiteral | VectorLiteral | MapLiteral)
    grammar_collapse=True

class FunctionLiteral(SquidGrammar):
    grammar = (REF('ArgList'), REF('Block'))
    grammar_tags = ("scope",)

    def on_declaration_pass(self, ns):
        self._scope = scope.Scope(parent=ns)
        return self._scope

    def on_check_types(self, context):
        self._llvm_return_type = None
        for r in self[1].find_all(Return):
            if self._llvm_return_type and r.get_type()._llvm_type is not self._llvm_return_type:
                raise TypeError("Return types are not of the same type")

            self._llvm_return_type = r.get_type()._llvm_type

        self._llvm_arg_types = ()
        for a in self[0].find_all(Identifier):
            args = args + (i8,)

        return None

    def generate_ir(self, context, builder, name):
        function_type = ir.FunctionType(self._llvm_return_type, self._llvm_arg_types)
        function = ir.Function(builder, function_type, name)
        self[1].generate_ir(context, function)

class SquidLiteral(SquidGrammar):
    grammar = (NumberLiteral 
             | HexLiteral 
             | BoolLiteral 
             | StringLiteral 
             | FunctionLiteral 
             | AggregateLiteral)
    grammar_collapse=True

#------------------------------

class TypeName(SquidGrammar):
    grammar = (REF('Identifier'))

class TypeConstructor(SquidGrammar):
    grammar = (TypeName, OPTIONAL('<', LIST_OF(REF('TypeExpr')),'>'))

class TypeGroup(SquidGrammar):
    grammar = (L('('), REF('TypeExpr'), L(')'))

class TypeVoid(SquidGrammar):
    grammar = (L('void'))

class TypeFunction(SquidGrammar):
    grammar = (L('|'), LIST_OF(REF('TypeExpr'), min=0), L('|'), L('->'), OPTIONAL(REF('TypeExpr')))

class TypeVector(SquidGrammar):
    grammar = (L('['), REF('TypeExpr'), L(']'))

class TypeUnitTuple(SquidGrammar):
    grammar = (L('('), REF('TypeExpr'), L(','), L(')'))
    grammar_collapse=True

class TypeNaryTuple(SquidGrammar):
    grammar = (L('('), LIST_OF(REF('TypeExpr'), min=2), L(')'))
    grammar_collapse=True

class TypeTuple(SquidGrammar):
    grammar = (TypeNaryTuple| TypeUnitTuple)

class TypeExpr(SquidGrammar):
    grammar = (TypeVoid | TypeName | TypeConstructor | TypeVector | TypeFunction)

class TypeDeclaration(SquidGrammar):
    grammar = (L('type'), TypeName, L('='), TypeExpr)

#------------------------------

class Identifier(SquidGrammar):
    grammar = (WORD('A-Za-z', 'A-Za-z0-9'))

class Binding(SquidGrammar):
    grammar = (Identifier, OPTIONAL((L(':'), TypeExpr)))
    
    def on_check_types(self, context):
        if self[1]:
            context.bind(self[0].string, self[1][1].get_type())
        else:
            context.bind(self[0].string, None)

        self._context = context
        return context.lookup(self[0].string)

    def set_type(self, t):
        self._type = t
        self._context.update(self[0].string, t)

class ArgList(SquidGrammar):
    grammar = (L('('), LIST_OF(Identifier, min=0), L(')'))

class Variable(SquidGrammar):
    grammar = (Identifier)

    def on_check_types(self, context):
        return context.lookup(self[0].string)
      
#------------------------------

class CallExpr(SquidGrammar):
    grammar = (Identifier, ArgsTupleLiteral)

class IndexExpr(SquidGrammar):
    grammar = (Identifier, VectorLiteral)

class GroupExpr(SquidGrammar):
    grammar = (L('('), REF('Expr'), L(')'))

    def on_check_types(self, context):
        return self[1].get_type()

#------------------------------

class UnaryPreOp(SquidGrammar):
    grammar = (L('-') | L('*') | L('~') | L('!'))

class UnaryPreOpExpr(SquidGrammar):
    grammar = (UnaryPreOp, REF('Expr'))
    grammar_collapse=True

class UnExpr(SquidGrammar):
    grammar = (UnaryPreOpExpr)

#------------------------------

class BinOp(SquidGrammar):
    pass

class BinOp0(BinOp):
    grammar = (L('*') | L('/') | L('%'))
 
class BinOp1(BinOp):
    grammar = (L('+') | L('-'))

class BinOp2(BinOp):
    #grammar = (L('<<') | L('>>'))
    grammar = (L('==') | L('!=') | L('<') | L('>') | L('<=') | L('>='))

class BinOp3(BinOp):
    grammar = (L('&'))

class BinOp4(BinOp):
    grammar = (L('^'))

class BinOp5(BinOp):
    grammar = (L('|'))

class BinOp6(BinOp):
    grammar = (L('==') | L('!=') | L('<') | L('>') | L('<=') | L('>='))

class BinOp7(BinOp):
    grammar = (L('&&'))

class BinOp8(BinOp):
    grammar = (L('||'))

#------------------------------

class BinTerm0(SquidGrammar):
    grammar = (SquidLiteral | Variable | UnExpr | CallExpr | IndexExpr | GroupExpr)

    def on_check_types(self, context):
        return self[0].get_type()

class BinExpr0(SquidBinExpr):
    grammar = (BinTerm0, ONE_OR_MORE(BinOp0, BinTerm0))

class BinTerm1(SquidGrammar):
    grammar = (BinExpr0 | BinTerm0)
    grammar_collapse=True

class BinExpr1(SquidBinExpr):
    grammar = (BinTerm1, ONE_OR_MORE(BinOp1, BinTerm1))

class BinTerm2(SquidGrammar):
    grammar = (BinExpr0 | BinExpr1 | BinTerm0)
    grammar_collapse=True

class BinExpr2(SquidBinExpr):
    grammar = (BinTerm2, ONE_OR_MORE(BinOp2, BinTerm2))

class BinTerm3(SquidGrammar):
    grammar = (BinExpr0 | BinExpr1 | BinExpr2 | BinTerm0)
    grammar_collapse=True

class BinExpr3(SquidGrammar):
    grammar = (BinTerm3, ONE_OR_MORE(BinOp3, BinTerm3))
    grammar_collapse=True

class BinTerm4(SquidGrammar):
    grammar = (BinExpr0 | BinExpr1 | BinExpr2 | BinExpr3 | BinTerm0)
    grammar_collapse=True

class BinExpr4(SquidGrammar):
    grammar = (BinTerm4, ONE_OR_MORE(BinOp4, BinTerm4))
    grammar_collapse=True

class BinTerm5(SquidGrammar):
    grammar = (BinExpr0 | BinExpr1 | BinExpr2 | BinExpr3 | BinExpr4 | BinTerm0)
    grammar_collapse=True

class BinExpr5(SquidGrammar):
    grammar = (BinTerm5, ONE_OR_MORE(BinOp5, BinTerm5))
    grammar_collapse=True

class BinTerm6(SquidGrammar):
    grammar = (BinExpr0 | BinExpr1 | BinExpr2 | BinExpr3 | BinExpr4 | BinExpr5 | BinTerm0)
    grammar_collapse=True

class BinExpr6(SquidGrammar):
    grammar = (BinTerm6, ONE_OR_MORE(BinOp6, BinTerm6))
    grammar_collapse=True

class BinTerm7(SquidGrammar):
    grammar = (BinExpr0 | BinExpr1 | BinExpr2 | BinExpr3 | BinExpr4 | BinExpr5 | BinExpr6 | BinTerm0)
    grammar_collapse=True

class BinExpr7(SquidGrammar):
    grammar = (BinTerm7, ONE_OR_MORE(BinOp7, BinTerm7))
    grammar_collapse=True

class BinTerm8(SquidGrammar):
    grammar = (BinExpr0 | BinExpr1 | BinExpr2 | BinExpr3 | BinExpr4 | BinExpr5 | BinExpr6 | BinExpr7 | BinTerm0)
    grammar_collapse=True

class BinExpr8(SquidGrammar):
    grammar = (BinTerm8, ONE_OR_MORE(BinOp8, BinTerm8))
    grammar_collapse=True

#------------------------------

# TODO Pratt parser - this is awful 2^n performance it seems...
class Expr(SquidGrammar):
    grammar = (#BinExpr8 
             #| BinExpr7 
             #| BinExpr6 
             #| BinExpr5 
             #| BinExpr4 
             #  BinExpr3 
               BinExpr2 
             | BinExpr1 
             | BinExpr0 
             | BinTerm0)

    def on_check_types(self, context):
        return self[0].get_type()

    def value(self):
        literal = self.find(NumberLiteral);
    
        if not literal:
            literal = self.find(BoolLiteral);

        return literal.value()
    

#------------------------------

class LetDeclaration(SquidGrammar):
    grammar = (L('let'), Binding, L('='), Expr)

    def on_check_types(self, context):
        t1 = self[1].get_type()
        t2 = self[3].get_type()
        if t1 and t2 and t1 != t2:
            raise TypeError("types don't match: " + t1._name + " and " + t2._name)
        
        if not t1:
            self[1].set_type(t2)

        return None

    def generate_ir(self, context, builder):
        func = self[3].find(FunctionLiteral)
        if func:
            func.generate_ir(context, builder, "main")
        

class VarDeclaration(SquidGrammar):
    grammar = (L('var'), Binding, L('='), Expr)

class DeclDeclaration(SquidGrammar):
    grammar = (L('decl'), Binding)

class Assignment(SquidGrammar):
    grammar = (Variable, L('='), Expr)

class Return(SquidGrammar):
    grammar = (L('return'), Expr)

    def on_check_types(self, context):
        return self[1].get_type()

    def generate_ir(self, context, builder):
        builder.ret(self[1].value())

class Comment(Grammar):
    grammar = (L('//'), REST_OF_LINE)

class Declaration(SquidGrammar):
    grammar = (DeclDeclaration | VarDeclaration | LetDeclaration | TypeDeclaration)
    grammar_collapse = True

class Else(SquidGrammar):
    grammar = (L('else'), REF('Block'))

class Elif(SquidGrammar):
    grammar = (L('elif'), Expr, REF('Block'))

class If(SquidGrammar):
    grammar = (L('if'), Expr, REF('Block'), ZERO_OR_MORE(Elif), OPTIONAL(Else))

    def generate_ir(self, context, builder):
        cond = self[1].value()
        with builder.if_then(cond):
            self[2].generate_ir(context, None, block=builder)

class Statement(SquidGrammar):
    grammar = (Comment | REF('Block') | Declaration | If | Assignment | Return | Expr, L(';'))

    def generate_ir(self, context, builder):
        self[0].generate_ir(context, builder)

class Block(SquidGrammar):
    grammar = (L('{'), ZERO_OR_MORE(Statement), L('}'))
    grammar_tags = ("scope",)

    def on_declaration_pass(self, ns):
        self._scope = scope.Scope(parent=ns)
        return self._scope

    def generate_ir(self, context, builder, block=None):
        if not block:
            block = builder.append_basic_block()
            block_builder = ir.IRBuilder(block)
        else:
            block_builder = block

        for stmt in self.find_all(Statement):
            stmt.generate_ir(context, block_builder)

class Module(SquidGrammar):
    grammar = (LIST_OF(Declaration, sep=';'))
    grammar_tags = ("scope",)

    def on_declaration_pass(self, ns):
        self._scope = scope.Scope(parent=ns)
        return self._scope

    def generate_ir(self, context, builder):
        self._llvm_module = ir.Module()
    
        for f in self.find_all(LetDeclaration):
            f.generate_ir(context, self._llvm_module)
        
        return self._llvm_module
