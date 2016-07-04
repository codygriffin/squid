from modgrammar import *
import scope 
import squid_types

from llvmlite import ir

i8 = ir.IntType(8)

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
                pass
                #raise TypeError("types don't match: " + str(t1) + " and " + str(t2))

        return t1


grammar_whitespace_mode = 'optional'

class NumberLiteral(SquidGrammar):
    grammar = (OPTIONAL('-'), WORD('0-9'), OPTIONAL('.', WORD('0-9')))

    def on_check_types(self, context):
        return squid_types.I32()

    def generate_ir(self, context, builder):
        return squid_types.I32.llvm_value(int(self.string))

class NilLiteral(SquidGrammar):
    grammar = (L('('), L(')'))

    def on_check_types(self, context):
        return squid_types.Void()

class HexLiteral(SquidGrammar):
    grammar = (L('0x') | L('0X'), WORD('A-Fa-f0-9'))

class BoolLiteral(SquidGrammar):
    grammar = (L('true') | L('false'))

    def on_check_types(self, context):
        return squid_types.Bool()

    def generate_ir(self, context, builder):
        if self.string == "true":
            return squid_types.Bool.llvm_value(True)
        else:
            return squid_types.Bool.llvm_value(False)

class StringLiteral(SquidGrammar):
    grammar = (L('\"'), ANY_EXCEPT('\"'), L('\"'))

    def encoded(self):
        return bytearray((self[1].string + '\0').encode('utf-8'))

    def on_check_types(self, context):
        return squid_types.Array(squid_types.I8(), len(self.encoded()))

    def generate_ir(self, context, builder):
        return ir.Constant(self._type.llvm_type(), self.encoded())

class UnitTupleLiteral(SquidGrammar):
    grammar = (L('('), REF('Expr'), L(','), L(')'))
    grammar_collapse=True

class NaryTupleLiteral(SquidGrammar):
    grammar = (L('('), LIST_OF(REF('Expr'), min=2), L(')'))
    grammar_collapse=True

class TupleLiteral(SquidGrammar):
    grammar = (NaryTupleLiteral | UnitTupleLiteral)

class ArgsTupleLiteral(SquidGrammar):
    grammar = (L('('), LIST_OF(REF('Expr'), min=0), L(')'))

class ArrayLiteral(SquidGrammar):
    grammar = (L('['), LIST_OF(REF('Expr'), min=0), L(']'))

class MapLiteral(SquidGrammar):
    grammar = (L('{'), LIST_OF(REF('Expr'), L(':'), REF('Expr')), L('}'))

class AggregateLiteral(SquidGrammar):
    grammar = (TupleLiteral | ArrayLiteral | MapLiteral)
    grammar_collapse=True

class FunctionLiteral(SquidGrammar):
    grammar = (REF('ArgList'), REF('Block'))
    grammar_tags = ("scope",)

    def on_declaration_pass(self, context):
        self._context = scope.Context(parent=context)
        return self._context

    def on_check_types(self, context):
        self._llvm_return_type = None
        for r in self[1].find_all(Return):
            if self._llvm_return_type and r.get_type().llvm_type() != self._llvm_return_type:
                raise TypeError("Return types are not of the same type")

            self._llvm_return_type = r.get_type().llvm_type()

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

class SquidType(SquidGrammar):
    def construct(self, context):
        return None

class TypeName(SquidType):
    grammar = (REF('Identifier'))

    def construct(self, context):
        return context.types().lookup(self.string)

class TypeConstructor(SquidType):
    grammar = (TypeName, OPTIONAL('<', LIST_OF(REF('TypeExpr')),'>'))

class TypeGroup(SquidType):
    grammar = (L('('), REF('TypeExpr'), L(')'))
    
    def construct(self, context):
        return self[1].construct(context)

class TypeFunction(SquidType):
    grammar = (L('('), LIST_OF(REF('TypeExpr'), min=0), L(')'), L('->'), REF('TypeExpr'))

    def construct(self, context):
        arg_types = self[1].find_all(TypeExpr)
        ret = self[4].construct(context) 
        return squid_types.Function(ret, map(lambda a: a.construct(context), arg_types))

class TypeArray(SquidType):
    grammar = (L('['), REF('TypeExpr'), L(']'))

class TypeUnitTuple(SquidType):
    grammar = (L('('), REF('TypeExpr'), L(','), L(')'))

    def construct(self, context):
        return squid_types.Tuple(self[1].construct(context))

class TypeNaryTuple(SquidType):
    grammar = (L('('), LIST_OF(REF('TypeExpr'), min=2), L(')'))

    def construct(self, context):
        types = self[1].find_all(TypeExpr)
        return squid_types.Tuple(map(lambda a: a.construct(context), types))

class TypeTuple(SquidType):
    grammar = (TypeNaryTuple | TypeUnitTuple)

    def construct(self, context):
        return  self[0].construct(context)

class TypeExpr(SquidType):
    grammar = (TypeName | TypeConstructor | TypeTuple | TypeArray | TypeFunction)

    def construct(self, context):
        return self[0].construct(context)

class TypeDeclaration(SquidType):
    grammar = (L('type'), TypeName, L('='), TypeExpr)

    def on_build_types(self, context):
        return context.types().bind(self[1].string, self[3].construct(context))

#------------------------------

class Identifier(SquidGrammar):
    grammar = (WORD('A-Za-z', 'A-Za-z0-9'))

class Binding(SquidGrammar):
    grammar = (Identifier, OPTIONAL((L(':'), TypeExpr)))

    def get_name(self):
        return self[0].string

    def get_type(self):
        if self[1]:
            return self[1][1]
        else:
            return None
    
    def on_check_types(self, context):
        if self[1]:
            context.values().bind(self[0].string, scope.Symbol(symtype=self[1][1].construct(context)))
        else:
            context.values().bind(self[0].string, scope.Symbol())

        self._context = context
        return context.values().lookup(self[0].string)._symtype

    def set_type(self, t):
        self._type = t
        self._context.values().lookup(self[0].string)._symtype = t

class ArgList(SquidGrammar):
    grammar = (L('('), LIST_OF(Identifier, min=0), L(')'))

    def on_build_types(self, context):
        for arg in self[1].find_all(Identifier):
            return context.bind(arg.string, None)

class Variable(SquidGrammar):
    grammar = (Identifier)

    def on_check_types(self, context):
        return context.values().lookup(self[0].string)._symtype

    def generate_ir(self, context, builder):
        return builder.load(context.values().lookup(self[0].string)._symvalue)

#------------------------------

class CallExpr(SquidGrammar):
    grammar = (Identifier, ArgsTupleLiteral)

    def on_check_types(self, context):
        func = context.values().lookup(self[0].string)
        return func._symtype._return_type

    def generate_ir(self, context, builder):
        print("calling function" + self[0].string)
        #return self[1].generate_ir(context, builder)

class IndexExpr(SquidGrammar):
    grammar = (Identifier, ArrayLiteral)

class GroupExpr(SquidGrammar):
    grammar = (L('('), REF('Expr'), L(')'))

    def on_check_types(self, context):
        return self[1].get_type()

    def generate_ir(self, context, builder):
        return self[1].generate_ir(context, builder)

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
    grammar = (L('*') | L('/') | L('%') |
               L('+') | L('-') |
               L('<<') | L('>>') |
               L('&') |
               L('^') |
               L('|') |
               L('==') | L('!=') | L('<') | L('>') | L('<=') | L('>=') |
               L('&&') |
               L('||'))

#------------------------------

class BinTerm(SquidGrammar):
    grammar = (SquidLiteral | Variable | UnExpr | CallExpr | IndexExpr | GroupExpr)

    def on_check_types(self, context):
        return self[0].get_type()

    def value(self):
        return self[0].value()

    def generate_ir(self, context, builder):
        return self[0].generate_ir(context, builder)

class BinExpr(SquidBinExpr):
    grammar = (BinTerm, ONE_OR_MORE(BinOp, BinTerm))
    # TODO Lookup should be by operator AND the types being operated on
    ops = {
        "+": lambda b, x, y: b.add(x,y),
        "*": lambda b, x, y: b.mul(x,y),
        "-": lambda b, x, y: b.sub(x,y),
        "/": lambda b, x, y: b.sdiv(x,y),
        "==": lambda b, x, y: b.icmp_signed("==", x,y),
        "!=": lambda b, x, y: b.icmp_signed("!=", x,y),
    }

    def generate_ir(self, context, builder):
        val = self[0].generate_ir(context, builder)
        for op in self[1].find_all(Grammar): 
            val2 = op[1].generate_ir(context, builder)
            val = BinExpr.ops[op[0].string](builder, val, val2)
        return val

#------------------------------

class Expr(SquidGrammar):
    grammar = (BinExpr | BinTerm)

    def on_check_types(self, context):
        return self[0].get_type()

    def generate_ir(self, context, builder):
        return self[0].generate_ir(context, builder)

#------------------------------

class LetDeclaration(SquidGrammar):
    grammar = (L('let'), Binding, L('='), Expr)

    def on_check_types(self, context):
        t1 = self[1].get_type()
        t2 = self[3].get_type()
        if t1 and t2 and t1 != t2:
            pass
            #raise TypeError("types don't match: " + t1._name + " and " + t2._name)
        
        if not t1:
            self[1].set_type(t2)

        return None

    def generate_ir(self, context, builder):
        # allocate a place on the stack for our var we are declaring
        ref = self[3].get_type().alloca(builder);
        context.values().lookup(self[1].get_name())._symvalue = ref
        builder.store(self[3].generate_ir(context, builder), ref)

class VarDeclaration(SquidGrammar):
    grammar = (L('var'), Binding, L('='), Expr)

class FnDeclaration(SquidGrammar):
    grammar = (L('fn'), Identifier, FunctionLiteral)

    def on_check_types(self, context):
        context.values().bind(self[1].string, scope.Symbol(symtype=self[2].get_type()))
        return self[2].get_type()

    def generate_ir(self, context, builder):
        print("gen fn")
        print(self[2])
        self[2].generate_ir(context, builder, self[1].string)

class Assignment(SquidGrammar):
    grammar = (Variable, L('='), Expr)

class Return(SquidGrammar):
    grammar = (L('return'), Expr)

    def on_check_types(self, context):
        return self[1].get_type()

    def generate_ir(self, context, builder):
        builder.ret(self[1].generate_ir(context, builder))

class Comment(Grammar):
    grammar = (L('//'), REST_OF_LINE)

    def generate_ir(self, context, builder):
        return None

class Declaration(SquidGrammar):
    grammar = (FnDeclaration | VarDeclaration | LetDeclaration | TypeDeclaration)

    def generate_ir(self, context, builder):
        self[0].generate_ir(context, builder)

class Else(SquidGrammar):
    grammar = (L('else'), REF('Block'))

class Elif(SquidGrammar):
    grammar = (L('elif'), Expr, REF('Block'))

class If(SquidGrammar):
    grammar = (L('if'), Expr, REF('Block'), ZERO_OR_MORE(Elif), OPTIONAL(Else))

    def generate_ir(self, context, builder):
        cond = self[1].generate_ir(context, builder)
        with builder.if_then(cond):
            self[2].generate_ir(context, None, block=builder)

class Statement(SquidGrammar):
    grammar = (Comment | REF('Block') | Declaration | If | Assignment | Return | Expr, L(';'))

    def generate_ir(self, context, builder):
        self[0].generate_ir(context, builder)

class Block(SquidGrammar):
    grammar = (L('{'), ZERO_OR_MORE(Statement), L('}'))
    grammar_tags = ("scope",)

    def on_declaration_pass(self, context):
        self._context = scope.Context(parent=context)
        return self._context

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

    def on_declaration_pass(self, context):
        self._context = scope.Context(parent=context)
        return self._context

    def generate_ir(self, context, builder):
        self._llvm_module = ir.Module()
    
        for f in self.find_all(Declaration):
            f.generate_ir(context, self._llvm_module)
        
        return self._llvm_module
