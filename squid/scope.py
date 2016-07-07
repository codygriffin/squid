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
        result = self._symbols.get(name, None)

        # Look up in parent scope if its not here
        if self._parent and not result:
            result = self._parent.lookup(name)

        if not self._parent and not result:
            raise Exception("Unknown symbol: " + name)

        return result 

class Context(object):
    def __init__(self, parent=None):
        if parent:
            self._types = Scope(parent._types)
            self._values = Scope(parent._values)
        else:
            self._types = Scope()
            self._values = Scope()

    def types(self):
        return self._types

    def values(self):
        return self._values

    def __str__(self):
        return "Types: " + str(self._types._symbols) + "\nValues: " + str(self._values._symbols)

class Symbol(object):
    def __init__(self, symvalue=None, symtype=None):
        self._symvalue = symvalue
        self._symtype = symtype
