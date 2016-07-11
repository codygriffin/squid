from modgrammar import *
from llvmlite import ir

from squid import scope
from squid import types
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


class Binding(Grammar, Typed):
    grammar = (Identifier, OPTIONAL(L(':'), TypeExpr))
    
    def infer_type(self, type_env):
        subst = {}
        binding_type = types.TypeVariable()
        type_env.extend(self.get_name(), binding_type)
        if self[1]:
            annotated_type = self[1][1].construct(type_env)
            subst = types.compose(subst, types.unify(annotated_type, binding_type))

        type_env.substitute(subst)

        return (subst, binding_type)

    def get_name(self):
        return self[0].string
    
    def on_check_types(self, context):
        return self[2].construct(context)

# XXX
class ArgList(Grammar):
    grammar = (L('('), LIST_OF(Binding, min=0), L(')'))

class LetDeclaration(SquidStatement, Typed):
    grammar = (L('let'), Binding, L('='), Expr)

    def infer_type(self, type_env):
        subst = {}
        subst_binding, binding_type = self[1].infer_type(type_env)
        subst_exp, exp_type = self[3].infer_type(type_env)
    
        type_env.substitute(types.compose(subst_binding, subst_exp, types.unify(binding_type, exp_type)))
    
        self._let_type = binding_type
        return (types.compose(subst_binding, subst_exp, types.unify(binding_type, exp_type)), None)

    def compile(self, builder, type_env, sym_table):
        print("compiling let")
        # allocate a place on the stack for our var we are declaring
        ref = type_env.lookup(self[1].get_name()).alloca(builder);
        sym_table[self[1].get_name()] = ref
        builder.store(self[3].compile(builder, type_env, sym_table), ref)

class VarDeclaration(SquidStatement):
    grammar = (L('var'), Binding, L('='), Expr)

class FnDeclaration(SquidStatement, Scoped, Typed):
    grammar = (L('fn'), Identifier, ArgList, OPTIONAL(L('->'), TypeExpr), OPTIONAL(REF('Block')))
    grammar_tags = ("scope",)

    def get_type_env(self):
        return self._fn_env

    def infer_type(self, type_env):
        subst = {}

        # We go ahead and pick some type variables for our declaration
        fn_type = types.TypeVariable()
        ret_type = types.TypeVariable()
        arg_types = []

        # We add the declared identifier to the type environment, and then
        # we create a new environment for the function
        type_env.extend(self[1].string, fn_type)
        self._fn_env = type_env.clone()

        # We infer the argument types
        for arg in self[2].find_all(Binding):
            subst_binding, binding_type = arg.infer_type(self._fn_env)
            arg_types = arg_types + [binding_type]
            subst = types.compose(subst, subst_binding)

        # If a return type is specified, we go ahead and try to unify it with
        # our return variable type
        if self[3]:
            ret_annotated_type = self[3].find(TypeExpr).construct(self._fn_env) 
            subst = types.compose(subst, types.unify(ret_type, ret_annotated_type))

        # If we have a body (this is a definition), then we go ahead and infer
        # the type of the block, and also unify this with the return type
        if self[4]:
            subst_body, body_type = self.find(Block).infer_type(self._fn_env)
            subst = types.compose(subst_body, types.unify(ret_type, body_type))

        # once we have some types for the return and args, we unify the actual function
        # type.
        subst = types.compose(subst, types.unify(fn_type, types.Function(ret_type, arg_types)))
        self._fn_env.substitute(subst)

        # TODO we need to ensure that any non-bound variables are updated in the 
        # parent environment...
        type_env.extend(self[1].string, self._fn_env.lookup(self[1].string))

        return (subst, None)

    def compile(self, builder, type_env, sym_table):
        print("compiling fn")
        # Add to our symbol table
        name = self[1].string
        function = ir.Function(builder, type_env.lookup(name).llvm_type(), name)
        sym_table[name] = function
        # TODO new scope!
        if self[4]:
            self.find(Block).compile(function, type_env, sym_table)

class ModuleDeclaration(SquidStatement):
    grammar = (L('module'), Identifier)

    def compile(self, builder, type_env, sym_table):
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

    def compile(self, builder, type_env, sym_table):
        return None 


class Declaration(SquidStatement):
    grammar = (FnDeclaration | VarDeclaration | LetDeclaration | TypeDeclaration | ModuleDeclaration)

    def compile(self, builder, type_env, sym_table):
        self[0].compile(builder, type_env, sym_table)

#------------------------------

class Assignment(SquidStatement):
    grammar = (Variable, L('='), Expr)

    def compile(self, builder, type_env, sym_table):
        # allocate a place on the stack for our var we are declaring
        builder.store(self[2].compile(builder, type_env, sym_table), sym_table[self[0].string])

class Return(SquidStatement):
    grammar = (L('return'), Expr)

    def infer_type(self, type_env):
        subst = {}
        
        # We assume we know nothing about the return type
        ret_type = types.TypeVariable()
        subst_exp, exp_type = self[1].infer_type(type_env)
        subst = types.compose(subst_exp, types.unify(ret_type, exp_type))
        type_env.substitute(subst)
        return (subst, ret_type)

    def compile(self, builder, type_env, sym_table):
        builder.ret(self[1].compile(builder, type_env, sym_table))

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

class Block(SquidStatement, Typed):
    grammar = (L('{'), ZERO_OR_MORE(REF('Statement')), L('}'))

    def infer_type(self, type_env):
        subst = {}

        fns = self[1].find_all(FnDeclaration)
        lets = self[1].find_all(LetDeclaration)
        exprs = self[1].find_all(Expr)
        rets = self[1].find_all(Return)
    
        for fn in fns:
            subst_fn, _ = fn.infer_type(type_env)
            subst = types.compose(subst, subst_fn)

        for let in lets:
            subst_let, _ = let.infer_type(type_env)
            subst = types.compose(subst, subst_let)

        for expr in exprs:
            subst_expr, _ = expr.infer_type(type_env)
            subst = types.compose(subst, subst_expr)

        ret_type = types.Void()
        for ret in rets:
            subst_ret, ret_type = ret.infer_type(type_env)
            subst = types.compose(subst, subst_ret)

        type_env.substitute(subst)

        # TODO sum types
        return (subst, ret_type)

    def compile(self, builder, type_env, sym_table, block=None):
        if not block:
            block = builder.append_basic_block()
            block_builder = ir.IRBuilder(block)
        else:
            block_builder = block

        for stmt in self.find_all(Statement):
            stmt.compile(block_builder, type_env, sym_table)

class Statement(SquidStatement):
    grammar = (Block | Declaration | If | Assignment | Return | Expr, L(';'))

    def compile(self, builder, type_env, sym_table):
        self[0].compile(builder, type_env, sym_table)
