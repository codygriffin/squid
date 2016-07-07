class SymbolTable(object):
'''
Store regex sorted by name and complexity.  The assumption
is that simple expressions will be more general than
complex expressions.  

A query string is then broken into a root and a suffix, where
the root is a specific character class (those which make up 
valid identifiers for example), and the suffix corresponds to
parameters.

If the query string matches a key expression, the value closure 
executes with the capture groups from the suffix matched from the 
query string.

In squid, symbol names are mangled into regex strings.  

Identifier  => Identifier           => fn callback() -> Symbol { /* */ };
Box<A>      => Box,([\d])           => fn callback(a: String) -> Symbol {}
Tuple<A...> => Tuple(,[\d])(,[\d])* => fn callback(a...: (String...)) -> Symbol {}

Suppose we have the following squid code:
type Tuple<A> = A 
type Tuple<A, B...> = A * Tuple<B...>

This defines two patterns which are matched by query strings to return
and execute the associated callback to compute the symbol.  This lazy 
symbol table allows sophisticated recursive type definitions.

The callback closes over the AST so that the syntax of the expression
is available - the args correspond to reified types.

The callback also receives its own symbol table, so that the parameters
can be bound without affecting the parent symbol table.
'''    
    def __init__(self, parent=None):
        self._parent = parent
        self._symbol_generators = {}

    def bind(self, key, symbol):
        if name in self._symbols:
            raise Exception("Duplicate symbol: " + name)

        self._symbols[key] = symbol        
        
    def lookup(self, name):
        # TODO find most general regex match
        result = self._symbols.get(name, None)

        # Look up in parent scope if its not here
        if self._parent and not result:
            result = self._parent.lookup(name)

        if not self._parent and not result:
            raise Exception("Unknown symbol: " + name)

        return result 
