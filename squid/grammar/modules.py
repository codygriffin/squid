from modgrammar import *

from squid import types
from squid.grammar import *
from squid.grammar.statements import Declaration, FnDeclaration, LetDeclaration

grammar_whitespace_mode = 'optional'

class SquidModule(Grammar):
    '''
    Modules are simply a collection of declarations, along with
    access specifiers.  
    '''
    pass

class Module(SquidModule, Typed, Scoped):
    grammar = (LIST_OF(Declaration, sep=';'))

    def infer_type(self):
        print("infering module")
        self._type_env = types.TypeEnvironment({
            "i8": types.Int(8),
            "i32": types.Int(32),
            "bool": types.Bool(),
        })

        subst = {}
        # we just start with declarations
        fns = self[0].find_all(FnDeclaration)
        lets = self[0].find_all(LetDeclaration)
    
        for fn in fns:
            subst_fn, _ = fn.infer_type(self._type_env)
            subst = types.compose(subst, subst_fn)
            
        for let in lets:
            subst_let, _ = let.infer_type(self._type_env)
            subst = types.compose(subst, subst_let)

        self._type_env.substitute(subst)
        return (subst, None)

    def get_type_env(self):
        return self._type_env

    def get_scope(self):
        pass
