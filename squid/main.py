from grammar import *
from scope import *

import sys

program = \
"""
type String = [i8];
decl puts : |String| -> i8;

let main = () {
    if (true) {
        return 0;
    };
    return 1;
};
"""

if __name__ == '__main__':
    parser = Module.parser()
    #parser = Expr.parser()
    try:
        # Create our root scope
        root = Scope()

        root.bind("void", Type("void"))
        root.bind("Vector", Type("Vector"))
        root.bind("i8", Type("i8"))
        root.bind("bool", Type("bool"))

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
            print(r._scope._symbols)
            for f in r.find_all("namespace"):
                dump_symbols(f)

        print("Dumping Symbols:")
        print(root._symbols)
        dump_symbols(result)

        # Print the module IR
        llvm.initialize()
        llvm.initialize_native_target()
        llvm.initialize_native_asmprinter()

        llvm_main = llvm.parse_assembly(str(module))
        tm = llvm.Target.from_default_triple().create_target_machine()

        print("Dumping IR:")
        print(str(module))
    
        with open('output.o', 'wb') as f:
            f.write(tm.emit_object(llvm_main))

    except Exception as e:
        print(e)
        raise e
