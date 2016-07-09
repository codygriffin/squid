class Typed(object):
    '''
    Typed nodes support type inference via Hindley-Milner
    '''
    def get_type(self, type_env=None):
        return types.Void()


class Compiled(object):
    '''
    Compiled nodes result in LLVM IR.  
    '''
    def compile(self):
        return None
