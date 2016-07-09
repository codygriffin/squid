from modgrammar import *
from squid import types

grammar_whitespace_mode = 'optional'

class SquidLiteral(Grammar):
    '''
    Literals are things like numbers, strings, constants
    that we parse and have values and types at compile
    time.  From literals, we infer the types of expressions
    in which they appear.
    '''
    def get_type(self):
        return types.Void()

    def get_value(self):
        raise Exception("Void has no value") 

class RealLiteral(SquidLiteral):
    grammar = (OPTIONAL('-'), WORD('0-9'), '.', OPTIONAL(WORD('0-9')))

    def get_type(self):
        return types.Float()

    def get_value(self):
        return float(self.string)

class IntLiteral(SquidLiteral):
    grammar = (OPTIONAL('-'), WORD('0-9'))

    def get_type(self):
        return types.Int64()

    def get_value(self):
        return int(self.string)

class HexLiteral(SquidLiteral):
    grammar = (L('0x') | L('0X'), WORD('A-Fa-f0-9'))

    def get_type(self):
        return types.Int64()

    def get_value(self):
        return int(self.string, 16)

class OctLiteral(SquidLiteral):
    grammar = (L('0') | L('0'), WORD('0-7'))

    def get_type(self):
        return types.Int64()

    def get_value(self):
        return int(self.string, 8)

class BoolLiteral(SquidLiteral):
    grammar = (L('true') | L('false'))

    def get_type(self):
        return types.Bool()

    def get_value(self):
        if self.string == "true":
            return True
        else:
            return False

class StringLiteral(SquidLiteral):
    grammar = (L('\"'), ANY_EXCEPT('\"'), L('\"'))

    def get_type(self):
        return types.Array(types.I8(), len(self.get_value()))

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

    def get_type(self):
        return self[0].get_type()

    def get_value(self):
        return self[0].get_value()
