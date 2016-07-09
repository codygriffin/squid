from modgrammar import *


class Identifier(Grammar):
    grammar = (WORD('A-Za-z_', 'A-Za-z0-9_'))

    def get_name(self):
        return self.string
