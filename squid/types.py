import copy
import string
from functools import reduce
from llvmlite import ir

from squid.symbols.symbol_table import SymbolTable

class Substitutable(object):
    '''
    This is a trait describing something can be substituted
    
    We can either apply a substitution, or get a list of all
    the free variables.
    '''
    def substitute(self, subst):
        '''
        Apply the substitution to the set of free variables
        '''
        pass

    def free_type_vars(self):
        '''
        Return the set of unbound, free variables 
        '''
        pass


def unify(t1, t2):
    '''
    Find the substitution which unifies two types
    '''
    # TODO clean up types?
    a = t1
    b = t2
    #print (str(a) + " unifying with " + str(b))
    if isinstance(a, TypeVariable):
        if a != b:
            if not isinstance(b, int) and a.occurs_in(b):
                raise Exception("recursive unification")
            a.instance = b
            return {a: b}
    elif isinstance(a, TypeOperator) and isinstance(b, TypeVariable):
        return unify(b, a)
    elif isinstance(a, TypeOperator) and isinstance(b, TypeOperator):
        if a != b:
            raise Exception("Type mismatch: {0} != {1}".format(str(a), str(b)))
        subst = {}
        for p, q in zip(a._params, b._params):
            subst.update(unify(p, q))
        return subst
    elif isinstance(a, int) and isinstance(b, TypeVariable):
        return unify(b, a)
    elif isinstance(a, int) and isinstance(b, int):
        if a != b:  
            raise Exception("Type (int) mismatch: {0} != {1}".format(str(a), str(b)))
        return {}
    else:
        assert 0, "Not unified: {0} != {1}".format(str(a), str(b))

def compose_subst(s1, s2):
    '''
    Create a new substitution equivalent to substituting s1 and then s2.
    '''
    subst = {v: s2[v].substitute(s1) for v in s2 if not isinstance(s2[v], int)}
    subst.update(s1) 
    return subst

def compose(*s):
    '''
    Compose an arbitrary number of substitutions
    '''
    return reduce(compose_subst, s,{})

def subst_to_string(subst):
    return "\n".join(["\t" + str(v) + " ==> " + str(t) for v, t in subst.items()])

class TypeEnvironment(Substitutable):
    '''
    The type environment maps identifiers to types.
    '''
    def __init__(self, type_vars={}):
        self._type_vars = type_vars

    def clone(self):
        return TypeEnvironment(type_vars=self._type_vars.copy())

    def extend(self, var, scheme):
        self._type_vars.update({
            var: scheme
        })

    def restrict(self, var):
        self._type_vars.remove(var)

    def lookup(self, var):
        return self._type_vars[var]
        
    def each(self, cb):
        for v, t in self._type_vars.items(): 
            cb(v, t);
        
    def substitute(self, subst):
        for key in self._type_vars:
            self._type_vars[key] = self._type_vars[key].substitute(subst)
                
    def free_type_vars(self):
        free_vars = set([])
        for k in self._type_vars.values():
            free_vars = free_vars | k.free_type_vars()

        return free_vars

    def __str__(self):
        return "\n".join(["\t" + str(v) + " => " + str(t) for v,t in self._type_vars.items()])

class Type(Substitutable):
    '''
    Type is our unit type - all types are sub-types of
    void.  Statements all return void.

    Type has no values. 
    '''
    def unify(self, other):
        if (self == other):
            return {}
        else:
            raise Exception("Unable to unify types!")

    def __str__(self):
        return type(self).__name__

    def substitute(self, subst):
        return self

    def free_type_vars(self):
        return set()

    def __hash__(self):
        return str(self).__hash__()


class TypeVariable(Type):
    '''
    A TypeVariable is used to fill out the type environment
    while performing type inference, or as place-holders for
    parameterized types.

    TypeVariables are numbered starting from 0, and have a 
    string "name" as %0, %1, etc
    '''
    next_id = 0
    def __init__(self):
        self._id = TypeVariable.next_id
        TypeVariable.next_id += 1

    def free_type_vars(self):
        '''
        For a type variable, by definition the variable is unbound, so
        the free variables is just the singleton set containing this 
        variable
        '''
        return set([self])

    def substitute(self, subst):
        '''
        Applying substitution to a variable either results in a replacement
        from the substitution map, or we remain unchanged
        '''
        return subst.get(self, self) 

    def occurs_in(self, typ):
        '''
        Test if this variable occurs in a type's free variables
        '''
        return self in typ.free_type_vars()

    def bind(self, typ):
        '''
        Bind this variable to a type
        '''
        if self == typ:
            return {}
        elif self.occurs_in(typ):
            raise Exception("Infinite Type!")
        else:
            return {self: typ}

    def __hash__(self):
        return self._id

    def __str__(self):
        return "%" + str(self._id)


class TypeOperator(Type):
    '''
    A TypeOperator builds a new type from existing types
    '''
    def __init__(self, typ, *params):
        self._type = typ
        self._params = list(params)

    def free_type_vars(self):
        return set(self._type.free_type_vars() - set(self._params))

    def substitute(self, subst):
        subst_free = {v: subst[v] for v in subst if v not in self._params}
        return TypeOperator(self._type.substitute(subst_free), self._params)

    def __str__(self):
        return type(self).__name__ + "<" + ", ".join(map(str, self._params)) + ">"

    def __hash__(self):
        return str(self).__hash__()
    
    def __eq__(self, other):
        '''
        Type schemes are equal iff they have the same root type, and the same number
        of parameters
        '''
        if not isinstance(other, TypeOperator):
            return False
        return self._type == other._type and len(self._params) == len(other._params)


# TODO should be a TypeOperator (creates function types from ret/args types)
class Function(TypeOperator):
    TYPE=Type()
    def __init__(self, ret, args):
        self._ret = ret
        self._args  = args 
        super().__init__(Function.TYPE, ret, *args)

    def free_type_vars(self):
        args = list(map(lambda t: t.free_type_vars(), self._args))
        return self._ret.free_type_vars() | reduce(set.union, args, set())

    def substitute(self, subst):
        args = list(map(lambda t: t.substitute(subst), self._args))
        ret = self._ret.substitute(subst)
        return Function(ret, args)


class Void(Type):
    def llvm_type(self):
        return ir.VoidType()
    
    @classmethod
    def llvm_value(self, value):
        raise Exception("No value for void")

class Bool(TypeOperator):
    TYPE=Type()
    def __init__(self):
        super().__init__(Bool.TYPE)

    def substitute(self, subst):
        return self

    def llvm_type(self):
        return ir.IntType(1)
    
    @classmethod
    def llvm_value(self, value):
        if value:
            return ir.Constant(ir.IntType(1), 1);
        else:
            return ir.Constant(ir.IntType(1), 0);


class Int(TypeOperator):
    TYPE=Type()
    def __init__(self, size=32):
        super().__init__(Int.TYPE, size)

    def substitute(self, subst):
        return self

    def llvm_type(self):
        return ir.IntType(32)

    @classmethod
    def llvm_value(self, value):
        return ir.Constant(ir.IntType(32), value);

class Float(TypeOperator):
    TYPE=Type()
    def __init__(self):
        super().__init__(Float.TYPE)

    def substitute(self, subst):
        return self

    def llvm_type(self):
        return ir.FloatType()

    @classmethod
    def llvm_value(self, value):
        return ir.Constant(ir.FloatType(), value);


class Array(TypeOperator):
    TYPE=Type()
    def __init__(self, t, count):
        self._type = t
        self._count = count
        super().__init__(Array.TYPE, t, count)

    def substitute(self, subst):
        return self

    def llvm_type(self):
        return ir.ArrayType(self._type.llvm_type(), self._count)

    def alloca(self, builder):
        return builder.alloca(self.llvm_type(), self._count)

class Box(TypeOperator):
    TYPE=Type()
    def __init__(self, t):
        self._type = t
        super().__init__(Box.TYPE, t)

    def substitute(self, subst):
        return self

    def llvm_type(self):
        return ir.PointerType(self._type.llvm_type())

    def alloca(self, builder):
        return builder.alloca(self.llvm_type())


class Tuple(TypeOperator):
    TYPE=Type()
    def __init__(self, types):
        self._types = list(types)
        super().__init__(Tuple.TYPE, *t)

    def llvm_type(self):
        return ir.LiteralStructType(map(llvm_type, self._types))

    def get(self, index):
        return self_types[index]
