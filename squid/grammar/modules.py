from modgrammar import *
from llvmlite import ir

from squid import types
from squid.grammar import *
from squid.grammar.statements import Declaration, FnDeclaration, LetDeclaration, ModuleDeclaration

grammar_whitespace_mode = 'optional'

class SquidModule(Grammar):
    '''
    Modules are simply a collection of declarations, along with
    access specifiers.  
    '''
    pass

class Module(SquidModule, Scoped):
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
        print(self._type_env)
        return (subst, None)

    def compile(self):
        names = self.find_all(ModuleDeclaration)
        if len(names) != 1:
            raise Exception('Module must have exactly one module declaration')

        self._llvm_module = ir.Module(name=names[0][1].string)
        self._sym_table = {}
    
        for f in self.find_all(Declaration):
            print("compiling" + str(f))
            f.compile(self._llvm_module, self._type_env, self._sym_table)
        
        return self._llvm_module

    def get_type_env(self):
        return self._type_env

    def get_sym_table(self):
        pass
