from modgrammar import *

from squid import types
from squid.grammar.statements import Declaration, FnDeclaration, LetDeclaration

grammar_whitespace_mode = 'optional'

class SquidModule(Grammar):
    '''
    Modules are simply a collection of declarations, along with
    access specifiers.  
    '''
    pass

class Module(SquidModule):
    grammar = (LIST_OF(Declaration, sep=';'))

    def infer_type(self):
        print("infering module")
        type_env = types.TypeEnvironment({
        })

        subst = {}
        # we just start with declarations
        fns = self[0].find_all(FnDeclaration)
        lets = self[0].find_all(LetDeclaration)
    
        for fn in fns:
            subst_fn, _ = fn.infer_type(type_env)
            subst.update(subst_fn)

        for let in lets:
            subst_let, _ = let.infer_type(type_env)
            subst.update(subst_let)

        print("before subbing")
        type_env.each(lambda v, t:
            print("\t" + str(v) + " ==> " + str(t)))

        print("after subbing")
        type_env.substitute(subst).each(lambda v, t:
            print("\t" + str(v) + " ==> " + str(t)))

        return (subst, None)

    def get_scope(self):
        pass
