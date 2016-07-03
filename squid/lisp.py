from modgrammar import *
import sys

grammar_whitespace_mode = 'optional'

symbols = {
  "print" : lambda *args: print(*args),
  "add" : lambda *args: sum(args),
  "+" : lambda *args: sum(args),
  "==" : lambda *args: all(x==args[0] for x in args),
  "dump" : lambda *args: print(symbols)
}

class Number (Grammar):
    grammar = (OPTIONAL('-'), WORD('0-9'), OPTIONAL('.', WORD('0-9')))

    def value(self, scope=symbols):
        return float(self.string)

class Bool (Grammar):
    grammar = (L('True') | L('False'))

    def value(self, scope=symbols):
        return bool(self.string)

class String (Grammar):
    grammar = (L('\"'), WORD('^\"'), L('\"'))

    def value(self, scope=symbols):
        return str(self[1])

class Identifier (Grammar):
    grammar = (WORD('A-Za-z_\-\+\\\*\=', 'A-Za-z0-9_\-\+\\\*\='))

    def value(self, scope=symbols):
        val = scope.get(self.string, None)
        if val is None:
            raise "undefined indentifier!"

        return val
    
class Atom (Grammar):
    grammar = (Bool | Number | Identifier | String)
  
    def value(self, scope=symbols):
        return self[0].value(scope);

class List (Grammar):
    grammar = ('(', REPEAT(REF('Expr'), min=0), ')')

    def value(self, scope=symbols):
        members = self[1]
        if len(members) == 0:
            return None

        func = scope[members[0].string]
        args = list(map(lambda e: e.value(scope), members.elements[1:]))
        return func(*args)

class Expr (Grammar):
    grammar = (Atom | List)

    def value(self, scope=symbols):
        return self[0].value(scope);

class Form (Grammar):
    grammar = (List)

    def value(self, scope=symbols):
        form = self[0][1]
        if form[0].string == 'def':
            return self.eval_def(form, scope)
        elif form[0].string == 'defn':
            return self.eval_defn(form, scope)
        elif form[0].string == 'if':
            return self.eval_if(form, scope)
        elif form[0].string == 'quote':
            return self.eval_quote(form, scope)
        else:
            return self[0].value(scope)

    def eval_if(self, form, scope):
        cond = form[1].value(scope)
        if cond:
            form[2].value(scope)
        else:
            form[3].value(scope)
        return None

    def eval_def(self, form, scope):
        var = form[1].string
        val = form[2].value(scope)
        symbols[var] = val
        return None

    def eval_quote(self, form, scope):
        return [form.elements[1:]]

    def eval_defn(self, form, scope):
        var = form[1].string
        args = form[2][0][1]
        body = form.elements[3:]

        # create a new scope
        funscope = scope.copy()

        # create a function and bind our args to the scope
        def fun(*funargs):
            for i, x in enumerate(args.elements):
                funscope[x[0].string] = funargs[i]
            return list(map(lambda e: e.value(funscope), body))[-1]

        # assign to our symbol table
        symbols[var] = fun
        return None

class Program (Grammar):
    grammar = (REPEAT(Form, min=0))

    def run(self):
        return list(map(lambda e: e.value(scope=symbols), self[0]))[-1]

parser = Program.parser(debug=sys.stdout)
more = False

while True:
    if more:
        sys.stdout.write(">>")
    else:
        sys.stdout.write(">")

    code = input()
    result = parser.parse_text(code, eof=True)
    if result:
        print(result.run())
        more=False;
    else:
        more=True;
