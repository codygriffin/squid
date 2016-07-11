from modgrammar import Grammar

from squid.grammar.modules import Module
from squid.grammar.expressions import *
from squid.grammar.statements import *
from squid.scope import *
from squid.types import *

import sys
import subprocess

from llvmlite import ir
import llvmlite.binding as llvm




# linker cmd: ld -o output output.o -read_only_relocs suppress -lc -lcrt1.o
program = \
"""
module main;
fn puts(str: [i8]) -> i32;

fn main () {
  let s = "hello from squid!";
  return puts(s[0]);
};
"""

if __name__ == '__main__':
    parser = Module.parser()
    #parser = Expr.parser()
    try:
        # Create our root scope
        result = parser.parse_text(program, eof=True)
        
        # Pass over AST and fill out namespaces
        #result.declaration_pass(root)

        # Pass over AST and check types
        #result.check_types(root)

        # Pass over AST and generate IR
        #module = result.generate_ir(root, None)

        def dump_ast(r, i=0):
            print(str(str(type(r)) + ": " + r.string).rjust(i*4))
            for f in r.find_all(Grammar):
                dump_ast(f, i+1)

        print("Dumping AST:")
        print(dump_ast(result))

        result.infer_type()
        result.get_type_env().each(lambda v, t:
            print(str(v) + " -> " + str(t)))

        sys.exit(1)


        print("Dumping Symbols:")

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
