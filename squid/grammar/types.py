from modgrammar import *
from squid import scope, squid_types

from squid.grammar.identifiers import Identifier 
from squid.grammar.literals import IntLiteral 

class SquidType(Grammar):
    def construct(self, context, *params):
        return None

class TypeConstructor(SquidType):
    grammar = (Identifier, 
      OPTIONAL(L('<'), LIST_OF(Identifier, min=0), L('>'))) 

    def name(self):
        suffix = ''
        for i, p in enumerate(self.params()):
          suffix += '_' + str(i)
        return self[0].string + suffix;

    def params(self):
        if self[1]:
            return list(map(lambda e: e.string, self[1].find_all(Identifier)))
        else:
            return []

    def construct(self, context, *params):
        if not self[1]:
            return context.types().lookup(self.name())
        else:
            params = list(map(lambda t: context.types().lookup(t), self.params()))
            print(self.name())
            print(context.types()._symbols)
            print(context.types().lookup(self.name()))
            return context.types().lookup(self.name())(*params)

class TypeGroup(SquidType):
    grammar = (L('('), REF('TypeExpr'), L(')'))
    
    def construct(self, context, *params):
        return self[1].construct(context)

class TypeFunction(SquidType):
    grammar = (L('('), LIST_OF(REF('TypeExpr'), min=0), L(')'), L('->'), REF('TypeExpr'))

    def construct(self, context, *params):
        arg_types = self[1].find_all(TypeExpr)
        ret = self[4].construct(context) 
        return squid_types.Function(ret, map(lambda a: a.construct(context), arg_types))

class TypeArray(SquidType):
    grammar = (L('['), REF('TypeExpr'), L(';'), IntLiteral, L(']'))

    def construct(self, context, *params):
        return squid_types.Array(self[1].construct(context), int(self[3].string))

class TypeBox(SquidType):
    grammar = (L('['), REF('TypeExpr'), L(']'))

    def construct(self, context, *params):
        return squid_types.Box(self[1].construct(context))

class TypeUnitTuple(SquidType):
    grammar = (L('('), REF('TypeExpr'), L(','), L(')'))

    def construct(self, context, *params):
        return squid_types.Tuple([self[1].construct(context)])

class TypeNaryTuple(SquidType):
    grammar = (L('('), LIST_OF(REF('TypeExpr'), min=2), L(')'))

    def construct(self, context, *params):
        types = self[1].find_all(TypeExpr)
        return squid_types.Tuple(map(lambda a: a.construct(context), types))

class TypeTuple(SquidType):
    grammar = (TypeNaryTuple | TypeUnitTuple)

    def construct(self, context, *params):
        return  self[0].construct(context)

class TypeBinOp(SquidType):
    grammar = (L('*') | L('-'))

#------------------------------

class TypeBinTerm(SquidType):
    grammar = (TypeConstructor | TypeTuple | TypeBox | TypeArray | TypeFunction)

    def construct(self, context, *params):
        return self[0].construct(context)

class TypeBinExpr(SquidType):
    grammar = (TypeBinTerm, ONE_OR_MORE(TypeBinOp, TypeBinTerm))

    def construct(self, context, *params):
        val1 = self[0].construct(context)
        for op in self[1].find_all(Grammar):
            val2 = op[1].construct(context, params)
            if op[0].string == '*':
                return squid_types.Tuple([val1, val2])
            
#------------------------------

class TypeExpr(SquidType):
    grammar = (TypeBinExpr | TypeBinTerm)

    def construct(self, context, *params):
        return self[0].construct(context)
