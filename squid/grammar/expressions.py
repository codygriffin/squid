from modgrammar import *
from squid import types

from squid.grammar import *
from squid.grammar.identifiers import Identifier
from squid.grammar.literals import Literal

grammar_whitespace_mode = 'optional'

class SquidExpr(Grammar, Typed, Compiled):
    '''
    Expressions have a type, and ultimately get
    turned into code.

    Because expressions can have variables, they must 
    be looked at in a context (symbol table and type
    environement, for example)
    
    Examples of expressions include binary/unary 
    operators, indexing, function application
    and even just simple literals.  Essentially,
    anything which evaluates to a value at run-time.
    '''
    def infer_type(self, type_env):
        raise Exception("cannot infer type of abstract expression")


class SquidAggregate(SquidExpr):
    '''
    Aggregates are collections of expressions.

    On top of normal expression operations, aggregates
    can be queried by an index
    '''
    pass

class Variable(SquidExpr):
    grammar = (Identifier)

    def infer_type(self, type_env):
        return ({}, type_env.lookup(self[0].string))

class UnitTuple(SquidAggregate):
    grammar = (L('('), REF('Expr'), L(','), L(')'))
    grammar_collapse=True

class NaryTuple(SquidAggregate):
    grammar = (L('('), LIST_OF(REF('Expr'), min=2), L(')'))
    grammar_collapse=True

class Tuple(SquidAggregate):
    grammar = (NaryTuple | UnitTuple)

    def get_type(self):
        types = map(lambda e: e.get_type(), self[0].find_all(Expr))
        return squid_types.Tuple(types)

class Array(SquidAggregate):
    grammar = (L('['), LIST_OF(REF('Expr'), min=0), L(']'))

    def infer_type(self, type_env):
        items = self[0][1]
    
        result = types.TypeVariabie()
        subst = {}
        subst_item = item_type = items[0].infer_type(type_env)
        subst.update(subst_item)

        # TODO - need to check all items...
        type_int = len(items.find_all(Expr))

        # Unify
        subst_result = types.unify(result, types.Array(item_type, type_int))
        subst.update(subst_result)
    
        return (subst, result)

    def get_type(self):
        types = list(map(lambda e: e.get_type(), self[1].find_all(Expr)))
        # TODO all types should match
        return squid_types.Array(types[0], len(types))

class ArgsTuple(Grammar):
    grammar = (L('('), LIST_OF(REF('Expr'), min=0), L(')'))

class Aggregate(SquidAggregate):
    grammar = (Tuple | Array)
    grammar_collapse=True

class CallExpr(SquidExpr):
    # TODO fix left-recursion 
    grammar = (Variable, ArgsTuple)

    def infer_type(self, type_env):
        fun = self[0]
        args = self[1][1].find_all(Expr)
    
        # Just allocate a variable for the result type
        result = types.TypeVariable()
        subst = {}

        # Infer the type of the function
        subst_fun, fun_type = fun.infer_type(type_env)

        # and the args
        arg_types = []
        for arg in args:
            subst_arg, arg_type = arg.infer_type(type_env)
            arg_types = arg_types + [arg_type]
            subst = types.compose(subst, subst_arg)
        

        # Unify the return value of the function with our result type
        subst = types.compose(subst_fun, subst_arg, types.unify(fun_type, types.Function(result, arg_types)))
        type_env.substitute(subst)

        return (subst, result)

    def on_check_types(self, context):
        func = context.values().lookup(self[0].string)
        return func._symtype._return_type

    def generate_ir(self, context, builder):
        func = context.values().lookup(self[0].string)._symvalue
        return builder.call(func, map(lambda a: a.generate_ir(context, builder), self[1].find_all(Expr)))

class IndexExpr(SquidExpr):
    # TODO fix left-recursion 
    grammar = (Variable, Array)

    def infer_type(self, type_env):
        array = self[0]
        indices = self[1][1]

        result = types.TypeVariable()
        subst_indices, indices_type = indices[0].infer_type(type_env)
        subst_array, array_type = array.infer_type(type_env)
        subst = types.compose(subst_indices, subst_array, types.unify(array_type, types.Array(result, types.TypeVariable())), types.unify(indices_type, types.Int()))

        type_env.substitute(subst)
        return (subst, result)

    def on_check_types(self, context):
        return self[0].get_type()

    def generate_ir(self, context, builder):
        ref = context.values().lookup(self[0].string)._symvalue
        return builder.gep(ref, [ir.Constant(ir.IntType(32), 0)] + self[1].as_list(context, builder))

class GroupExpr(SquidExpr):
    grammar = (L('('), REF('Expr'), L(')'))

    def on_check_types(self, context):
        return self[1].get_type()

    def generate_ir(self, context, builder):
        return self[1].generate_ir(context, builder)

#------------------------------

class UnaryPreOp(SquidExpr):
    grammar = (L('-') | L('*') | L('~') | L('!'))

class UnaryPreOpExpr(SquidExpr):
    grammar = (UnaryPreOp, REF('Expr'))
    grammar_collapse=True

class UnExpr(SquidExpr):
    grammar = (UnaryPreOpExpr)

#------------------------------

class BinOp(SquidExpr):
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

class BinTerm(SquidExpr):
    grammar = (Literal | Variable | Aggregate | UnExpr | GroupExpr | CallExpr | IndexExpr)

    def infer_type(self, type_env):
        return self[0].infer_type(type_env)

    def on_check_types(self, context):
        return self[0].get_type()

    def value(self):
        return self[0].value()

    def generate_ir(self, context, builder):
        return self[0].generate_ir(context, builder)

class BinExpr(SquidExpr):
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

    def infer_type(self, type_env):
        subst, t1 = self.get_lhs().infer_type(type_env)
        for i in range(self.count_rhs()):
            s2, t2 = self.get_rhs(i).infer_type(type_env)
            subst = types.compose(subst, s2, types.unify(t1, t2)) 

        type_env.substitute(subst)
        return subst, t1

#------------------------------

class Expr(SquidExpr):
    grammar = (BinExpr | BinTerm)
    grammar_tags = ('typed')

    def infer_type(self, type_env):
        return self[0].infer_type(type_env)

    def get_free_type_vars(self, type_env=None):
        return self[0].get_free_type_vars(type_env)
