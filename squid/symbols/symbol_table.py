import re
from collections import OrderedDict

class SymbolTable(object):
    '''
    A query string is broken into a root and a suffix, where
    the root is a specific character class (those which make up 
    valid identifiers for example), and the suffix corresponds to
    symbol parameters.

    The root is used to look up a list of matches, and the best
    match is chosen based on the parameters.

    Example for type symbols:

    Variant<A> = A
    Variant<A, B...> = A + Variant<B...>

    We need to add two symbols so we can match the appropriate type
    definition.

    Variant(,[\d]) 
    Variant(,[\d])(,[\d])+ 

    For the first case, any symbol lookup which matches the regex will 
    return a function which evaluates to the given parameter.  

    ***Declarations must be made in the order that matching will occur.***

    Variant<Bool> is mangled to Variant,0; as it only has one paramters.  
    This will match the first symbol and return the appropriate symbol
    generator which takes the type given by Bool as a parameter. This 
    generator simply returns the type given by the parameter.

    Variant<Bool, String, I8> would be mangled to Variant,0,1,2; which 
    would match the second symbol and return a symbol generator which
    takes Bool as the first parameters, and the tuple (String, I8) as
    the second.  This generator will return the type given by the sum
    of Bool and the Variant<String, I8>.  Variant<String, I8> is mangled
    to Variant,0,1; which also matches the second paramater and returns
    the same symbol generator, this time with the parameters as String
    and (I8,).  This generator will return the type given by the sum
    of String and Variant<I8>, which matches the first symbol and 
    yields I8.

    The regex matching seems like overkill until you consider that we may 
    mangle parameters of different kinds (ints, functions, types), and
    we can get some sophisticated matching behavior.
    '''    
    def __init__(self, parent=None):
        self._parent = parent
        self._symbol_generators = {}

    def declare(self, root, callback, *args):
        if root not in self._symbol_generators:
            self._symbol_generators[root] = OrderedDict() 

        suffix = ""
        for i, a in enumerate(args):
          suffix += a
        suffix += "$"

        self._symbol_generators[root][suffix] = callback
          
    def lookup(self, root, *args):
        result = self._lookup_local(root, *args)
  
        if not result and self._parent:
            result = self._parent.lookup(root, *args)

        return result
  
    def dump(self):
        for root, value in self._symbol_generators.items(): 
            print("Symbols beginning with " + root)
            for symbol, value in value.items(): 
                print(" -----> " + root + symbol)

    def _lookup_local(self, root, *args):
        symbol_list = self._symbol_generators[root]

        suffix = ""
        for i, a in enumerate(args):
          suffix += "," + str(i)

        for regex, value in symbol_list.items():
            if re.match(regex, suffix):
                return value(*args)

        return None
  


if __name__ == '__main__':
    symtable = SymbolTable()

    def callback1(a):
        return a

    def callback2(a, *b):
        return (a, symtable.lookup("Variant", *b))

    symtable.declare("Variant", callback1, "(,[\d])")
    symtable.declare("Variant", callback2, "(,[\d])", "(,[\d])*")
    print(symtable.lookup("Variant", str, int, bool))
    symtable.dump()
