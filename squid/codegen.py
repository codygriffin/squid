from llvmlite import ir
import llvmlite.binding as llvm

# Create some useful types
i32 = ir.IntType(32)
fnty = ir.FunctionType(i32, (i32, i32))

# Create an empty module...
main = ir.Module(name="main")

# and declare a function named "fpadd" inside it
func = ir.Function(main, fnty, name="main")

# Now implement the function
block = func.append_basic_block()
builder = ir.IRBuilder(block)
builder.ret(ir.Constant(i32, 42))

# Print the module IR
llvm.initialize()
llvm.initialize_native_target()
llvm.initialize_native_asmprinter()

llvm_main = llvm.parse_assembly(str(main))
tm = llvm.Target.from_default_triple().create_target_machine()

print (main)

#with open("output", "wb") as output:
#    output.write(tm.emit_object(llvm_module))
