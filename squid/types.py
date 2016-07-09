from llvmlite import ir

from squid.symbols.symbol_table import SymbolTable

# XXX notes from http://dev.stephendiehl.com/fun/006_hindley_milner.html
class TypeEnvironment(SymbolTable):
    '''
    The type environment maps identifiers to types.
    '''
    pass

class Type(object):
    '''
    Type is our unit type - all types are sub-types of
    void.  Statements all return void.

    Type has no values.
    '''
    def llvm_type(self):
        pass

    def get_size(self, m):
        return self.llvm_type().get_abi_size(m)

    def alloca(self, builder):
        return builder.alloca(self.llvm_type())
    
    # XXX this isn't right
    def __eq__(self, other):
        return self.__class__ == other.__class__


class TypeVariable(object):
    '''
    A TypeVariable is used to fill out the type environment
    while performing type inference, or as place-holders for
    parameterized types.

    TypeVariables are numbered starting from 0, and have a 
    string "name" as %0, %1, etc
    '''
    next_id = 0
    def __init__(self):
        self._id = next_id
        TypeVariable.next_id += 1
    
        self._instance = None

    def name(self):
        return "%" + str(self._id)


class TypeConstructor(object):
    '''
    A TypeConstructor builds a new type from existing types
    '''
    def __init__(self, *params):
        self._params = params


# TODO should be a TypeConstructor (creates function types from ret/args types)
class Function(Type):
    def __init__(self, ret, args):
        self._return_type = ret
        self._arg_tuple = args 

    def llvm_type(self):
        return ir.FunctionType(self._return_type.llvm_type(), tuple(map(lambda e: e.llvm_type(), self._arg_tuple)))

    def alloca(self, builder):
        return None

    def instantiate(self, name, builder):
        return ir.Function(builder, self.llvm_type(), name)


class Void(Type):
    def llvm_type(self):
        return ir.VoidType()
    
    @classmethod
    def llvm_value(self, value):
        raise Exception("No value for void")

class Bool(Type):
    def llvm_type(self):
        return ir.IntType(1)
    
    @classmethod
    def llvm_value(self, value):
        if value:
            return ir.Constant(ir.IntType(1), 1);
        else:
            return ir.Constant(ir.IntType(1), 0);


class I8(Type):
    def llvm_type(self):
        return ir.IntType(8)

    @classmethod
    def llvm_value(self, value):
        return ir.Constant(ir.IntType(8), value);


class I16(Type):
    def llvm_type(self):
        return ir.IntType(16)

    @classmethod
    def llvm_value(self, value):
        return ir.Constant(ir.IntType(16), value);


class I32(Type):
    def llvm_type(self):
        return ir.IntType(32)

    @classmethod
    def llvm_value(self, value):
        return ir.Constant(ir.IntType(32), value);


class I64(Type):
    def llvm_type(self):
        return ir.IntType(64)

    @classmethod
    def llvm_value(self, value):
        return ir.Constant(ir.IntType(64), value);


class Float(Type):
    def llvm_type(self):
        return ir.FloatType()

    @classmethod
    def llvm_value(self, value):
        return ir.Constant(ir.FloatType(), value);


class Array(Type):
    def __init__(self, t, count):
        self._type = t
        self._count = count

    def llvm_type(self):
        return ir.ArrayType(self._type.llvm_type(), self._count)

    def alloca(self, builder):
        return builder.alloca(self.llvm_type(), self._count)

class Box(Type):
    def __init__(self, t):
        self._type = t

    def llvm_type(self):
        return ir.PointerType(self._type.llvm_type())

    def alloca(self, builder):
        return builder.alloca(self.llvm_type())


class Tuple(Type):
    def __init__(self, types):
        self._types = list(types)

    def llvm_type(self):
        return ir.LiteralStructType(map(lambda e: e.llvm_type(), self._types))

    def get(self, index):
        return self_types[index]


