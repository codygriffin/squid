from modgrammar import *

from squid.grammar.statements import Declaration

grammar_whitespace_mode = 'optional'

class SquidModule(Grammar):
    '''
    Modules are simply a collection of declarations, along with
    access specifiers.  
    '''
    pass

class Module(SquidModule):
    grammar = (LIST_OF(Declaration, sep=';'))

    def get_scope(self):
        pass
