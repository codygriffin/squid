class Typed(object):
    '''
    Typed nodes support type inference via Hindley-Milner
    '''
    def infer_type(self, type_env):
        raise


class Compiled(object):
    '''
    Compiled nodes result in LLVM IR.  
    '''
    def compile(self):
        raise
