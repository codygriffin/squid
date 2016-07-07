class SquidTerminal():
    pass

class Identifier(SquidGrammar):
    grammar = (WORD('A-Za-z', 'A-Za-z0-9'))

class NumberLiteral(SquidGrammar):
    grammar = (OPTIONAL('-'), WORD('0-9'), OPTIONAL('.', WORD('0-9')))

    def on_check_types(self, context):
        return squid_types.I32()

    def generate_ir(self, context, builder):
        return squid_types.I32.llvm_value(int(self.string))

class HexLiteral(SquidGrammar):
    grammar = (L('0x') | L('0X'), WORD('A-Fa-f0-9'))

    def on_check_types(self, context):
        return squid_types.I32()

    def generate_ir(self, context, builder):
        return squid_types.I32.llvm_value(int(self.string, 16))

class BoolLiteral(SquidGrammar):
    grammar = (L('true') | L('false'))

    def on_check_types(self, context):
        return squid_types.Bool()

    def generate_ir(self, context, builder):
        if self.string == "true":
            return squid_types.Bool.llvm_value(True)
        else:
            return squid_types.Bool.llvm_value(False)

class StringLiteral(SquidGrammar):
    grammar = (L('\"'), ANY_EXCEPT('\"'), L('\"'))

    def encoded(self):
        return bytearray((self[1].string + '\0').encode('utf-8'))

    def on_check_types(self, context):
        return squid_types.Array(squid_types.I8(), len(self.encoded()))

    def generate_ir(self, context, builder):
        return ir.Constant(self._type.llvm_type(), self.encoded())

class UnitTupleLiteral(SquidGrammar):
    grammar = (L('('), REF('Expr'), L(','), L(')'))

class NaryTupleLiteral(SquidGrammar):
    grammar = (L('('), LIST_OF(REF('Expr'), min=2), L(')'))

class TupleLiteral(SquidGrammar):
    grammar = (NaryTupleLiteral | UnitTupleLiteral)

    def on_check_types(self, context):
        return squid_types.Tuple(map(lambda e: e.get_type(), self[0].find_all(Expr)))

    def generate_ir(self, context, builder):
        value = tuple(map(lambda e: e.generate_ir(context, builder), self[0].find_all(Expr)))
        return ir.Constant(self._type.llvm_type(), value)

class ArgsTupleLiteral(SquidGrammar):
    grammar = (L('('), LIST_OF(REF('Expr'), min=0), L(')'))

class ArrayLiteral(SquidGrammar):
    grammar = (L('['), LIST_OF(REF('Expr'), min=0), L(']'))

    def on_check_types(self, context):
        types = list(map(lambda e: e.get_type(), self[1].find_all(Expr)))
        # TODO all types should match
        return squid_types.Array(types[0], len(types))

    def generate_ir(self, context, builder):
        value = list(map(lambda e: e.generate_ir(context, builder), self[1].find_all(Expr)))
        return ir.Constant(self.get_type().llvm_type(), value)

    def as_list(self, context, builder):
        return list(map(lambda e: e.generate_ir(context, builder), self[1].find_all(Expr)))

class MapLiteral(SquidGrammar):
    grammar = (L('{'), LIST_OF(REF('Expr'), L(':'), REF('Expr')), L('}'))

class AggregateLiteral(SquidGrammar):
    grammar = (TupleLiteral | ArrayLiteral | MapLiteral)
    grammar_collapse=True

class FunctionLiteral(SquidGrammar):
    grammar = (REF('ArgList'), REF('Block'))
    grammar_tags = ("scope",)

    def on_declaration_pass(self, context):
        self._context = scope.Context(parent=context)
        return self._context

    def on_check_types(self, context):
        self._llvm_return_type = None
        for r in self[1].find_all(Return):
            if self._llvm_return_type and r.get_type().llvm_type() != self._llvm_return_type:
                raise TypeError("Return types are not of the same type")

            self._llvm_return_type = r.get_type().llvm_type()

        self._llvm_arg_types = ()
        for a in self[0].find_all(Identifier):
            args = args + ()

        return None

    def generate_ir(self, context, builder, name):
        function_type = ir.FunctionType(self._llvm_return_type, self._llvm_arg_types)
        function = ir.Function(builder, function_type, name)
        self[1].generate_ir(context, function)

class SquidLiteral(SquidGrammar):
    grammar = (NumberLiteral 
             | HexLiteral 
             | BoolLiteral 
             | StringLiteral 
             | FunctionLiteral 
             | AggregateLiteral)
    grammar_collapse=True
