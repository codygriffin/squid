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
    def get_type(self, type_env=None):
        return types.Void()


class SquidAggregate(SquidExpr):
    '''
    Aggregates are collections of expressions.

    On top of normal expression operations, aggregates
    can be queried by an index
    '''
    pass

class Variable(SquidExpr):
    grammar = (Identifier)

    def get_type(self, context):
        # look up type in type environment
        return context.values().lookup(self[0].string)._symtype

    def generate_ir(self, context, builder):
        return builder.load(context.values().lookup(self[0].string)._symvalue)

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

    def on_check_types(self, context):
        func = context.values().lookup(self[0].string)
        return func._symtype._return_type

    def generate_ir(self, context, builder):
        func = context.values().lookup(self[0].string)._symvalue
        return builder.call(func, map(lambda a: a.generate_ir(context, builder), self[1].find_all(Expr)))

class IndexExpr(SquidExpr):
    # TODO fix left-recursion 
    grammar = (Variable, Array)

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

class Expr(SquidExpr):
    grammar = (BinExpr | BinTerm)

    def on_check_types(self, context):
        return self[0].get_type()

    def generate_ir(self, context, builder):
        return self[0].generate_ir(context, builder)