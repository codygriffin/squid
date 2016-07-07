from grammar import *
from scope import *
from squid_types import *

import subprocess

from llvmlite import ir
import llvmlite.binding as llvm


#type Power<A, B...> = (A, Power<B...>);

# linker cmd: ld -o output output.o -read_only_relocs suppress -lc -lcrt1.o
program = \
"""
module main;

type Power<A> = (A,);

fn puts(str: [i8]) -> i32;

fn main () -> i32 {
  let x : Power<i32> = (1,);
  let s : [i8;17] = "hello from squid!";
  return puts(s[0]);
};

"""

if __name__ == '__main__':
    parser = Module.parser()
    #parser = Expr.parser()
    try:
        # Create our root scope
        root = Context()

        root.types().bind("void", Void())
        root.types().bind("i8", I8())
        root.types().bind("i16", I16())
        root.types().bind("i32", I32())
        root.types().bind("i64", I64())
        root.types().bind("bool", Bool())

        result = parser.parse_text(program, eof=True)
        
        # Pass over AST and fill out namespaces
        result.declaration_pass(root)

        # Pass over AST and check types
        result.check_types(root)

        # Pass over AST and generate IR
        module = result.generate_ir(root, None)

        def dump_ast(r, i=0):
            print(str(str(type(r)) + ": " + r.string).rjust(i*4))
            for f in r.find_all(SquidGrammar):
                dump_ast(f, i+1)

        print("Dumping AST:")
        #print(dump_ast(result))

        def dump_symbols(r):
            print(r._context)
            for f in r.find_all("namespace"):
                dump_symbols(f)

        print("Dumping Symbols:")
        print(root)
        dump_symbols(result)

        print("Dumping IR:")
        print(str(module))
        module.triple = "x86_64-apple-darwin14.5.0"

       # llvm.initialize()
       # llvm.initialize_native_target()
       # llvm.initialize_native_asmprinter()

       # llvm_main = llvm.parse_assembly(str(module))
       # tm = llvm.Target.from_default_triple().create_target_machine()
    
       # with open('output.o', 'wb') as f:
       #     f.write(tm.emit_object(llvm_main))

        with open('output.ir', 'wb') as f:
            f.write(str(module).encode('ascii'))

        subprocess.call(['/usr/local/opt/llvm/bin/lli', 'output.ir'])

    except Exception as e:
        print(e)
        raise e
