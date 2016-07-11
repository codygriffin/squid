class Typed(object):
    '''
    Typed nodes support type inference via Hindley-Milner
    '''
    def infer_type(self, type_env):
        raise Exception("can't infer abstract type")


class Scoped(object):
    '''
    Scoped nodes store a symbol table and a type environment.
    '''
    def get_type_env(self):
        raise Exception("can't get type environment")

    def get_symbol_table(self):
        raise Exception("can't get symbol table")


class Compiled(object):
    '''
    Compiled nodes result in LLVM IR.  
    '''
    def compile(self, builder, type_env, sym_table):
        raise
