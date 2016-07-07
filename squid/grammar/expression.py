class SquidExpr():
    pass

#------------------------------

class CallExpr(SquidGrammar):
    # TODO fix left-recursion for first-class functions
    grammar = (Variable, ArgsTupleLiteral)

    def on_check_types(self, context):
        func = context.values().lookup(self[0].string)
        return func._symtype._return_type

    def generate_ir(self, context, builder):
        func = context.values().lookup(self[0].string)._symvalue
        return builder.call(func, map(lambda a: a.generate_ir(context, builder), self[1].find_all(Expr)))

class IndexExpr(SquidGrammar):
    grammar = (Variable, ArrayLiteral)

    def on_check_types(self, context):
        return self[0].get_type()

    def generate_ir(self, context, builder):
        ref = context.values().lookup(self[0].string)._symvalue
        return builder.gep(ref, [ir.Constant(ir.IntType(32), 0)] + self[1].as_list(context, builder))

class GroupExpr(SquidGrammar):
    grammar = (L('('), REF('Expr'), L(')'))

    def on_check_types(self, context):
        return self[1].get_type()

    def generate_ir(self, context, builder):
        return self[1].generate_ir(context, builder)

#------------------------------

class UnaryPreOp(SquidGrammar):
    grammar = (L('-') | L('*') | L('~') | L('!'))

class UnaryPreOpExpr(SquidGrammar):
    grammar = (UnaryPreOp, REF('Expr'))
    grammar_collapse=True

class UnExpr(SquidGrammar):
    grammar = (UnaryPreOpExpr)

#------------------------------

class BinOp(SquidGrammar):
    grammar = (L('*') | L('/') | L('%') |
               L('+') | L('-') |
               L('<<') | L('>>') |
               L('&') |
               L('^') |
               L('|') |
               L('==') | L('!=') | L('<') | L('>') | L('<=') | L('>=') |
               L('&&') |
               L('||'))

#------------------------------

class BinTerm(SquidGrammar):
    grammar = (SquidLiteral | Variable | UnExpr | GroupExpr | CallExpr | IndexExpr)

    def on_check_types(self, context):
        return self[0].get_type()

    def value(self):
        return self[0].value()

    def generate_ir(self, context, builder):
        return self[0].generate_ir(context, builder)

class BinExpr(SquidGrammar):
    grammar = (BinTerm, ONE_OR_MORE(BinOp, BinTerm))
    # TODO Lookup should be by operator AND the types being operated on
    ops = {
        '+': lambda b, x, y: b.add(x,y),
        '*': lambda b, x, y: b.mul(x,y),
        '-': lambda b, x, y: b.sub(x,y),
        '/': lambda b, x, y: b.sdiv(x,y),
        '==': lambda b, x, y: b.icmp_signed('==', x,y),
        '!=': lambda b, x, y: b.icmp_signed('!=', x,y),
    }

    def generate_ir(self, context, builder):
        val = self[0].generate_ir(context, builder)
        for op in self[1].find_all(Grammar): 
            val2 = op[1].generate_ir(context, builder)
            val = BinExpr.ops[op[0].string](builder, val, val2)
        return val

    def count_rhs(self):
        return len(self.elements[1].elements)

    def get_rhs(self, i):
        return self.elements[1][i].elements[1]

    def get_lhs(self):
        return self.elements[0]

    def get_op(self):
        return self.elements[1][0].elements[0]

    def on_build_types(self, context):
        pass

    def on_check_types(self, context):
        t1 = self.get_lhs().get_type()

        for i in range(self.count_rhs()):
            t2 = self.get_rhs(i).get_type()
            if t1 != t2:
                raise TypeError("types don't match: " + str(t1) + " and " + str(t2))

        return t1

#------------------------------

class Expr(SquidGrammar):
    grammar = (BinExpr | BinTerm)

    def on_check_types(self, context):
        return self[0].get_type()

    def generate_ir(self, context, builder):
        return self[0].generate_ir(context, builder)
