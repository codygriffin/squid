from modgrammar import *
from llvmlite import ir

from squid.grammar import *
from squid import types

grammar_whitespace_mode = 'optional'

class SquidLiteral(Grammar, Typed, Compiled):
    '''
    Literals are things like numbers, strings, constants
    that we parse and have values and types at compile
    time.  From literals, we infer the types of expressions
    in which they appear.
    '''
    def infer_type(self, type_env):
        raise Exception("can't infer type of abstract literal")

    def get_value(self):
        raise Exception("can't extract value of abstract literal")

    def compile(self, builder, type_env, sym_table):
        print('compiling literal')
        return ir.Constant(self.infer_type(type_env)[1].llvm_type(), self.get_value())

class RealLiteral(SquidLiteral):
    grammar = (OPTIONAL('-'), WORD('0-9'), '.', OPTIONAL(WORD('0-9')))

    def infer_type(self, type_env):
        return ({}, types.Float())

    def get_value(self):
        return float(self.string)

class IntLiteral(SquidLiteral):
    grammar = (OPTIONAL('-'), WORD('0-9'))

    def infer_type(self, type_env):
        return ({}, types.Int())

    def get_value(self):
        return int(self.string)

class HexLiteral(SquidLiteral):
    grammar = (L('0x') | L('0X'), WORD('A-Fa-f0-9'))

    def infer_type(self, type_env):
        return ({}, types.Int())

    def get_value(self):
        return int(self.string, 16)

class OctLiteral(SquidLiteral):
    grammar = (L('0') | L('0'), WORD('0-7'))

    def infer_type(self, type_env):
        return ({}, types.Int())

    def get_value(self):
        return int(self.string, 8)

class BoolLiteral(SquidLiteral):
    grammar = (L('true') | L('false'))

    def infer_type(self, type_env):
        return ({}, types.Bool())

    def get_value(self):
        if self.string == "true":
            return True
        else:
            return False

class StringLiteral(SquidLiteral):
    grammar = (L('\"'), ANY_EXCEPT('\"'), L('\"'))

    def infer_type(self, type_env):
        return ({}, types.Array(types.Int(8), len(self.get_value())))

    def get_value(self):
        return bytearray((self[1].string + '\0').encode('utf-8'))

class Literal(SquidLiteral):
    grammar = (OctLiteral 
             | HexLiteral 
             | RealLiteral 
             | IntLiteral 
             | BoolLiteral 
             | StringLiteral)
    grammar_collapse=True

    def infer_type(self, type_env):
        return self[0].infer_type(type_env)

    def get_value(self):
        return self[0].get_value()
