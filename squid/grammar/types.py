from modgrammar import *
from squid import scope, types

from squid.grammar.identifiers import Identifier 
from squid.grammar.literals import IntLiteral 

class SquidType(Grammar):
    def construct(self, type_env, *params):
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

    def construct(self, type_env, *params):
        if not self[1]:
            return type_env.lookup(self.name())
        else:
            params = list(map(lambda t: type_env.types().lookup(t), self.params()))
            print(self.name())
            print(type_env.types()._symbols)
            print(type_env.types().lookup(self.name()))
            return type_env.types().lookup(self.name())(*params)

class TypeGroup(SquidType):
    grammar = (L('('), REF('TypeExpr'), L(')'))
    
    def construct(self, type_env, *params):
        return self[1].construct(type_env)

class TypeFunction(SquidType):
    grammar = (L('('), LIST_OF(REF('TypeExpr'), min=0), L(')'), L('->'), REF('TypeExpr'))

    def construct(self, type_env, *params):
        arg_types = self[1].find_all(TypeExpr)
        ret = self[4].construct(type_env) 
        return types.Function(ret, list(map(lambda a: a.construct(type_env), arg_types)))

class TypeArray(SquidType):
    grammar = (L('['), REF('TypeExpr'), L(';'), IntLiteral, L(']'))

    def construct(self, type_env, *params):
        return types.Array(self[1].construct(type_env), int(self[3].string))

class TypeBox(SquidType):
    grammar = (L('['), REF('TypeExpr'), L(']'))

    def construct(self, type_env, *params):
        return types.Box(self[1].construct(type_env))

class TypeUnitTuple(SquidType):
    grammar = (L('('), REF('TypeExpr'), L(','), L(')'))

    def construct(self, type_env, *params):
        return types.Tuple([self[1].construct(type_env)])

class TypeNaryTuple(SquidType):
    grammar = (L('('), LIST_OF(REF('TypeExpr'), min=2), L(')'))

    def construct(self, type_env, *params):
        types = self[1].find_all(TypeExpr)
        return types.Tuple(map(lambda a: a.construct(type_env), types))

class TypeTuple(SquidType):
    grammar = (TypeNaryTuple | TypeUnitTuple)

    def construct(self, type_env, *params):
        return  self[0].construct(type_env)

class TypeBinOp(SquidType):
    grammar = (L('*') | L('-'))

#------------------------------

class TypeBinTerm(SquidType):
    grammar = (TypeConstructor | TypeTuple | TypeBox | TypeArray | TypeFunction)

    def construct(self, type_env, *params):
        return self[0].construct(type_env)

class TypeBinExpr(SquidType):
    grammar = (TypeBinTerm, ONE_OR_MORE(TypeBinOp, TypeBinTerm))

    def construct(self, type_env, *params):
        val1 = self[0].construct(type_env)
        for op in self[1].find_all(Grammar):
            val2 = op[1].construct(type_env, params)
            if op[0].string == '*':
                return types.Tuple([val1, val2])
            
#------------------------------

class TypeExpr(SquidType):
    grammar = (TypeBinExpr | TypeBinTerm)

    def construct(self, type_env, *params):
        return self[0].construct(type_env)
