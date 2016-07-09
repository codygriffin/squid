from modgrammar import *
from squid import scope, squid_types

from squid.grammar import literals, identifiers, expressions, statements

from llvmlite import ir

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

grammar_whitespace_mode = 'optional'

class Identifier(SquidGrammar):
    grammar = (WORD('A-Za-z', 'A-Za-z0-9'))

class NumberLiteral(SquidGrammar):
    grammar = (OPTIONAL('-'), WORD('0-9'), OPTIONAL('.', WORD('0-9')))

    def on_check_types(self, context):
        return squid_types.I32()

    def generate_ir(self, context, builder):
        return squid_types.I32.llvm_value(int(self.string))

class HexLiteral(SquidGrammar):
    grammar = (L('0x') | L('0X'), WORD('A-Fa-f0-9'))

    def on_check_types(self, context):
        return squid_types.I32()

    def generate_ir(self, context, builder):
        return squid_types.I32.llvm_value(int(self.string, 16))

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

class NaryTupleLiteral(SquidGrammar):
    grammar = (L('('), LIST_OF(REF('Expr'), min=2), L(')'))

class TupleLiteral(SquidGrammar):
    grammar = (NaryTupleLiteral | UnitTupleLiteral)

    def on_check_types(self, context):
        return squid_types.Tuple(map(lambda e: e.get_type(), self[0].find_all(Expr)))

    def generate_ir(self, context, builder):
        value = tuple(map(lambda e: e.generate_ir(context, builder), self[0].find_all(Expr)))
        return ir.Constant(self._type.llvm_type(), value)

class ArgsTupleLiteral(SquidGrammar):
    grammar = (L('('), LIST_OF(REF('Expr'), min=0), L(')'))

class ArrayLiteral(SquidGrammar):
    grammar = (L('['), LIST_OF(REF('Expr'), min=0), L(']'))

    def on_check_types(self, context):
        types = list(map(lambda e: e.get_type(), self[1].find_all(Expr)))
        # TODO all types should match
        return squid_types.Array(types[0], len(types))

    def generate_ir(self, context, builder):
        value = list(map(lambda e: e.generate_ir(context, builder), self[1].find_all(Expr)))
        return ir.Constant(self.get_type().llvm_type(), value)

    def as_list(self, context, builder):
        return list(map(lambda e: e.generate_ir(context, builder), self[1].find_all(Expr)))

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
            args = args + ()

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
    def construct(self, context, *params):
        return None

class TypeConstructor(SquidType):
    grammar = (Identifier, 
      OPTIONAL(L('<'), LIST_OF(Identifier, min=0), L('>'))) 

    def name(self):
        suffix = ''
        for i, p in enumerate(self.params()):
          suffix += '_' + str(i)
        return self[0].string + suffix;

    def params(self):
        if self[1]:
            return list(map(lambda e: e.string, self[1].find_all(Identifier)))
        else:
            return []

    def construct(self, context, *params):
        if not self[1]:
            return context.types().lookup(self.name())
        else:
            params = list(map(lambda t: context.types().lookup(t), self.params()))
            print(self.name())
            print(context.types()._symbols)
            print(context.types().lookup(self.name()))
            return context.types().lookup(self.name())(*params)

class TypeGroup(SquidType):
    grammar = (L('('), REF('TypeExpr'), L(')'))
    
    def construct(self, context, *params):
        return self[1].construct(context)

class TypeFunction(SquidType):
    grammar = (L('('), LIST_OF(REF('TypeExpr'), min=0), L(')'), L('->'), REF('TypeExpr'))

    def construct(self, context, *params):
        arg_types = self[1].find_all(TypeExpr)
        ret = self[4].construct(context) 
        return squid_types.Function(ret, map(lambda a: a.construct(context), arg_types))

class TypeArray(SquidType):
    grammar = (L('['), REF('TypeExpr'), L(';'), NumberLiteral, L(']'))

    def construct(self, context, *params):
        return squid_types.Array(self[1].construct(context), int(self[3].string))

class TypeBox(SquidType):
    grammar = (L('['), REF('TypeExpr'), L(']'))

    def construct(self, context, *params):
        return squid_types.Box(self[1].construct(context))

class TypeUnitTuple(SquidType):
    grammar = (L('('), REF('TypeExpr'), L(','), L(')'))

    def construct(self, context, *params):
        return squid_types.Tuple([self[1].construct(context)])

class TypeNaryTuple(SquidType):
    grammar = (L('('), LIST_OF(REF('TypeExpr'), min=2), L(')'))

    def construct(self, context, *params):
        types = self[1].find_all(TypeExpr)
        return squid_types.Tuple(map(lambda a: a.construct(context), types))

class TypeTuple(SquidType):
    grammar = (TypeNaryTuple | TypeUnitTuple)

    def construct(self, context, *params):
        return  self[0].construct(context)

class TypeBinOp(SquidType):
    grammar = (L('*') | L('-'))

#------------------------------

class TypeBinTerm(SquidType):
    grammar = (TypeConstructor | TypeTuple | TypeBox | TypeArray | TypeFunction)

    def construct(self, context, *params):
        return self[0].construct(context)

class TypeBinExpr(SquidType):
    grammar = (TypeBinTerm, ONE_OR_MORE(TypeBinOp, TypeBinTerm))

    def construct(self, context, *params):
        val1 = self[0].construct(context)
        for op in self[1].find_all(Grammar):
            val2 = op[1].construct(context, params)
            if op[0].string == '*':
                return squid_types.Tuple([val1, val2])
            
#------------------------------

class TypeExpr(SquidType):
    grammar = (TypeBinExpr | TypeBinTerm)

    def construct(self, context, *params):
        return self[0].construct(context)

class TypeDeclaration(SquidGrammar):
    grammar = (L('type'), TypeConstructor, L('='), TypeExpr)

    def on_build_types(self, context):
        if len(self[1].params()) == 0:
            # if we don't have parameters, this is just a simple alias
            return context.types().bind(self[1].name(), self[3].construct(context))
        else:
            # otherwise, we build a closure which will construct a type 
            # with parameters
            def type_ctor(*args):
                # we create a new scope when we define a new type, so we 
                # can bind free parameters to our arguments
                type_context = scope.Context(parent=context)
                for p in self[1].params():
                    type_context.types().bind(p, args[0])
                return self[3].construct(type_context, *args)

            return context.types().bind(self[1].name(), type_ctor)

    def generate_ir(self, context, builder):
        return None 

#------------------------------

class Binding(SquidGrammar):
    grammar = (Identifier, L(':'), TypeExpr)

    def get_name(self):
        return self[0].string
    
    def on_check_types(self, context):
        return self[2].construct(context)

class ArgList(SquidGrammar):
    grammar = (L('('), LIST_OF(Binding, min=0), L(')'))

    def on_build_types(self, context):
        pass

class Variable(SquidGrammar):
    grammar = (Identifier)

    def on_check_types(self, context):
        return context.values().lookup(self[0].string)._symtype

    def generate_ir(self, context, builder):
        return builder.load(context.values().lookup(self[0].string)._symvalue)

#------------------------------

class CallExpr(SquidGrammar):
    # TODO fix left-recursion for first-class functions
    grammar = (Variable, ArgsTupleLiteral)

    def on_check_types(self, context):
        func = context.values().lookup(self[0].string)
        return func._symtype._return_type

    def generate_ir(self, context, builder):
        func = context.values().lookup(self[0].string)._symvalue
        return builder.call(func, map(lambda a: a.generate_ir(context, builder), self[1].find_all(Expr)))

class IndexExpr(SquidGrammar):
    grammar = (Variable, ArrayLiteral)

    def on_check_types(self, context):
        return self[0].get_type()

    def generate_ir(self, context, builder):
        ref = context.values().lookup(self[0].string)._symvalue
        return builder.gep(ref, [ir.Constant(ir.IntType(32), 0)] + self[1].as_list(context, builder))

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
    grammar = (SquidLiteral | Variable | UnExpr | GroupExpr | CallExpr | IndexExpr)

    def on_check_types(self, context):
        return self[0].get_type()

    def value(self):
        return self[0].value()

    def generate_ir(self, context, builder):
        return self[0].generate_ir(context, builder)

class BinExpr(SquidGrammar):
    grammar = (BinTerm, ONE_OR_MORE(BinOp, BinTerm))
    # TODO Lookup should be by operator AND the types being operated on
    ops = {
        '+': lambda b, x, y: b.add(x,y),
        '*': lambda b, x, y: b.mul(x,y),
        '-': lambda b, x, y: b.sub(x,y),
        '/': lambda b, x, y: b.sdiv(x,y),
        '==': lambda b, x, y: b.icmp_signed('==', x,y),
        '!=': lambda b, x, y: b.icmp_signed('!=', x,y),
    }

    def generate_ir(self, context, builder):
        val = self[0].generate_ir(context, builder)
        for op in self[1].find_all(Grammar): 
            val2 = op[1].generate_ir(context, builder)
            val = BinExpr.ops[op[0].string](builder, val, val2)
        return val

    def count_rhs(self):
        return len(self.elements[1].elements)

    def get_rhs(self, i):
        return self.elements[1][i].elements[1]

    def get_lhs(self):
        return self.elements[0]

    def get_op(self):
        return self.elements[1][0].elements[0]

    def on_build_types(self, context):
        pass

    def on_check_types(self, context):
        t1 = self.get_lhs().get_type()

        for i in range(self.count_rhs()):
            t2 = self.get_rhs(i).get_type()
            if t1 != t2:
                raise TypeError("types don't match: " + str(t1) + " and " + str(t2))

        return t1

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
            raise TypeError("types don't match: " + str(t1) + " and " + str(t2))

        context.values().bind(self[1].get_name(), scope.Symbol(symtype=t1))

        return None

    def generate_ir(self, context, builder):
        # allocate a place on the stack for our var we are declaring
        ref = self[3].get_type().alloca(builder);
        context.values().lookup(self[1].get_name())._symvalue = ref
        builder.store(self[3].generate_ir(context, builder), ref)

class VarDeclaration(SquidGrammar):
    grammar = (L('var'), Binding, L('='), Expr)

class FnDeclaration(SquidGrammar):
    grammar = (L('fn'), Identifier, ArgList, OPTIONAL(L('->'), TypeExpr), OPTIONAL(REF('Block')))
    grammar_tags = ("scope",)

    def on_declaration_pass(self, context):
        self._context = scope.Context(parent=context)
        return self._context

    def on_check_types(self, context):
        ret = squid_types.Void()
        if self[3]:
            ret = self[3][1].construct(context)  
        args = map(lambda t: t.get_type(), self[2].find_all(Binding))
        t = squid_types.Function(ret, args)
        context.values().bind(self[1].string, scope.Symbol(symtype=t))
        return t

    def generate_ir(self, context, builder):
        function = self.get_type().instantiate(self[1].string, builder)
        context.values().lookup(self[1].string)._symvalue = function
        if self[4]:
            self[4].generate_ir(context, function)

class ModuleDeclaration(SquidGrammar):
    grammar = (L('module'), Identifier)

    def generate_ir(self, context, builder):
        pass

class Declaration(SquidGrammar):
    grammar = (FnDeclaration | VarDeclaration | LetDeclaration | TypeDeclaration | ModuleDeclaration)

    def generate_ir(self, context, builder):
        self[0].generate_ir(context, builder)


#------------------------------

class Assignment(SquidGrammar):
    grammar = (Variable, L('='), Expr)

    def generate_ir(self, context, builder):
        # allocate a place on the stack for our var we are declaring
        builder.store(self[2].generate_ir(context, builder), context.values().lookup(self[0].string)._symvalue)

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
        names = self.find_all(ModuleDeclaration)
        if len(names) != 1:
            raise Exception('Module must have exactly one module declaration')
        self._llvm_module = ir.Module(name=names[0][1].string)
    
        for f in self.find_all(Declaration):
            f.generate_ir(context, self._llvm_module)
        
        return self._llvm_module
