from llvmlite import ir

class Void(object):
    def llvm_type(self):
        return ir.VoidType()

    def get_size(self, m):
        return self.llvm_type().get_abi_size(m)

    def alloca(self, builder):
        return builder.alloca(self.llvm_type())


class Bool(Void):
    def llvm_type(self):
        return ir.IntType(1)
    
    @classmethod
    def llvm_value(self, value):
        if value:
            return ir.Constant(ir.IntType(1), 1);
        else:
            return ir.Constant(ir.IntType(1), 0);


class I8(Void):
    def llvm_type(self):
        return ir.IntType(8)

    @classmethod
    def llvm_value(self, value):
        return ir.Constant(ir.IntType(8), value);


class I16(Void):
    def llvm_type(self):
        return ir.IntType(16)

    @classmethod
    def llvm_value(self, value):
        return ir.Constant(ir.IntType(16), value);


class I32(Void):
    def llvm_type(self):
        return ir.IntType(32)

    @classmethod
    def llvm_value(self, value):
        return ir.Constant(ir.IntType(32), value);


class I64(Void):
    def llvm_type(self):
        return ir.IntType(64)

    @classmethod
    def llvm_value(self, value):
        return ir.Constant(ir.IntType(64), value);


class Array(Void):
    def __init__(self, t, count):
        self._type = t
        self._count = count

    def llvm_type(self):
        return ir.ArrayType(self._type.llvm_type(), self._count)

    def alloca(self, builder):
        return builder.alloca(self.llvm_type(), self._count)

class Pointer(Void):
    def __init__(self, t):
        self._type = t

    def llvm_type(self):
        return ir.PointerType(self._type.llvm_type())

    def alloca(self, builder):
        return builder.alloca(self.llvm_type())


class Tuple(Void):
    def __init__(self, types):
        self._types = types

    def llvm_type(self):
        return ir.LiteralStructType(map(lambda e: e.llvm_type(), self._types))

    def get(self, index):
        return self_types[index]


class Function(Void):
    def __init__(self, ret, args):
        self._return_type = ret
        self._arg_tuple = args 

    def llvm_type(self):
        return ir.FunctionType(self._return_type.llvm_type(), tuple(map(lambda e: e.llvm_type(), self._arg_tuple)))

    def alloca(self, builder):
        return None

    def instantiate(name, builder):
        return ir.Function(builder, self.llvm_type(), name)
