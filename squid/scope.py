class Symbol(object):
    def __init__(self, name=None, symtype=None, symconst=False, symref=False):
        pass

class Scope(object):
    def __init__(self, parent=None):
        self._parent  = parent
        self._symbols = {}
        
    def bind(self, name, symbol):
        if name in self._symbols:
            raise Exception("Duplicate symbol: " + name)

        self._symbols[name] = symbol        

    def update(self, name, symbol):
        if name not in self._symbols:
            raise Exception("Unknown symbol: " + name)

        self._symbols[name] = symbol        

    def lookup(self, name):
        if name not in self._symbols:
            raise Exception("Unknown symbol: " + name)

        result = self._symbols.get(name, None)

        # Look up in parent scope if its not here
        if self._parent and not result:
            result = self._parent.lookup(name)

        return result 
