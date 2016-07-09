from modgrammar import *

from squid import scope
from squid.grammar import *
from squid.grammar.identifiers import Identifier
from squid.grammar.expressions import Expr, Variable
from squid.grammar.types import TypeExpr, TypeConstructor

grammar_whitespace_mode = 'optional'

class SquidStatement(Grammar, Compiled):
    '''
    Statements are similar to expressions, except
    they do not return a value (or you could consider
    them as expressions with a void type.

    Statements are primarily used to declare new
    variables, types or functions as well providing
    traditional imperative control structures.
    '''
    pass


class Binding(Grammar):
    grammar = (Identifier, OPTIONAL(L(':'), TypeExpr))

    def get_name(self):
        return self[0].string
    
    def on_check_types(self, context):
        return self[2].construct(context)

# XXX
class ArgList(Grammar):
    grammar = (L('('), LIST_OF(Binding, min=0), L(')'))

class LetDeclaration(SquidStatement):
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

class VarDeclaration(SquidStatement):
    grammar = (L('var'), Binding, L('='), Expr)

class FnDeclaration(SquidStatement):
    grammar = (L('fn'), Identifier, ArgList, OPTIONAL(L('->'), TypeExpr), OPTIONAL(REF('Block')))
    grammar_tags = ("scope",)

    def declaration_pass(self, ns):
        ns = self.on_declaration_pass(ns)

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

class ModuleDeclaration(SquidStatement):
    grammar = (L('module'), Identifier)

    def generate_ir(self, context, builder):
        pass

class TypeDeclaration(SquidStatement):
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


class Declaration(SquidStatement):
    grammar = (FnDeclaration | VarDeclaration | LetDeclaration | TypeDeclaration | ModuleDeclaration)
    grammar_collapse=True

    def generate_ir(self, context, builder):
        self[0].generate_ir(context, builder)

#------------------------------

class Assignment(SquidStatement):
    grammar = (Variable, L('='), Expr)

    def generate_ir(self, context, builder):
        # allocate a place on the stack for our var we are declaring
        builder.store(self[2].generate_ir(context, builder), context.values().lookup(self[0].string)._symvalue)

class Return(SquidStatement):
    grammar = (L('return'), Expr)

    def on_check_types(self, context):
        return self[1].get_type()

    def generate_ir(self, context, builder):
        builder.ret(self[1].generate_ir(context, builder))

class LineComment(Grammar):
    grammar = (L('//'), REST_OF_LINE)

    def generate_ir(self, context, builder):
        return None

class BlockComment(Grammar):
    grammar = (L('/*'), ANY, L('*/'))

    def generate_ir(self, context, builder):
        return None

class Comment(Grammar):
    grammar = (LineComment | BlockComment)

class Else(SquidStatement):
    grammar = (L('else'), REF('Block'))

class Elif(SquidStatement):
    grammar = (L('elif'), Expr, REF('Block'))

class If(SquidStatement):
    grammar = (L('if'), Expr, REF('Block'), ZERO_OR_MORE(Elif), OPTIONAL(Else))

    def generate_ir(self, context, builder):
        cond = self[1].generate_ir(context, builder)
        with builder.if_then(cond):
            self[2].generate_ir(context, None, block=builder)

class Block(SquidStatement):
    grammar = (L('{'), ZERO_OR_MORE(REF('Statement')), L('}'))

    def declaration_pass(self, ns):
        ns = self.on_declaration_pass(ns)

    def declare(self, scope):
        self._scope = scope.Context(parent=context)
        return self._scope

    def generate_ir(self, context, builder, block=None):
        if not block:
            block = builder.append_basic_block()
            block_builder = ir.IRBuilder(block)
        else:
            block_builder = block

        for stmt in self.find_all(Statement):
            stmt.generate_ir(context, block_builder)

class Statement(SquidStatement):
    grammar = (Block | Declaration | If | Assignment | Return | Expr, L(';'))
    grammar_collapse=True

    def generate_ir(self, context, builder):
        self[0].generate_ir(context, builder)

