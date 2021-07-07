# -*- coding: utf-8 -*-
"""
This awkward set of tests tries to make the tool bang its head against iself.
"""
from __future__ import generator_stop

import json
import os
import pickle
import shutil
import sys
import unittest
import py_compile

from tatsu.walkers import DepthFirstWalker
from tatsu.parser import GrammarGenerator, EBNFParser
from tatsu.parser_semantics import EBNFGrammarSemantics
from tatsu.codegen import codegen
from tatsu import util
from tatsu.util import asjson


tmp = os.path.abspath('./tmp')
sys.path.insert(0, str(tmp))


class BootstrapTests(unittest.TestCase):

    def test_bootstrap(self):
        print()

        if os.path.isfile('./tmp/00.ast'):
            shutil.rmtree('./tmp')
        if not os.path.isdir('./tmp'):
            os.mkdir('./tmp')
        print('-' * 20, 'phase 00 - parse using the bootstrap grammar')
        with open('grammar/tatsu.ebnf') as f:
            text = str(f.read())
        g = EBNFParser('EBNFBootstrap')
        grammar0 = g.parse(text)
        ast0 = json.dumps(asjson(grammar0), indent=2)
        with open('./tmp/00.ast', 'w') as f:
            f.write(ast0)

        print('-' * 20, 'phase 01 - parse with parser generator')
        with open('grammar/tatsu.ebnf') as f:
            text = str(f.read())
        g = GrammarGenerator('EBNFBootstrap')
        result = g.parse(text)

        generated_grammar1 = str(result)
        with open('./tmp/01.ebnf', 'w') as f:
            f.write(generated_grammar1)

        print('-' * 20, 'phase 02 - parse previous output with the parser generator')
        with open('./tmp/01.ebnf') as f:
            text = str(f.read())
        g = GrammarGenerator('EBNFBootstrap')
        result = g.parse(text)
        generated_grammar2 = str(result)
        with open('./tmp/02.ebnf', 'w') as f:
            f.write(generated_grammar2)
        self.assertEqual(generated_grammar2, generated_grammar1)

        print('-' * 20, 'phase 03 - repeat')
        with open('./tmp/02.ebnf') as f:
            text = f.read()
        g = EBNFParser('EBNFBootstrap')
        ast3 = g.parse(text)
        with open('./tmp/03.ast', 'w') as f:
            f.write(json.dumps(asjson(ast3), indent=2))

        print('-' * 20, 'phase 04 - repeat')
        with open('./tmp/02.ebnf') as f:
            text = f.read()
        g = GrammarGenerator('EBNFBootstrap')
        parser = g.parse(text)
    #    pprint(parser.first_sets, indent=2, depth=3)
        generated_grammar4 = str(parser)
        with open('./tmp/04.ebnf', 'w') as f:
            f.write(generated_grammar4)
        self.assertEqual(generated_grammar4, generated_grammar2)

        print('-' * 20, 'phase 05 - parse using the grammar model')
        with open('./tmp/04.ebnf') as f:
            text = f.read()
        ast5 = parser.parse(text)
        with open('./tmp/05.ast', 'w') as f:
            f.write(json.dumps(asjson(ast5), indent=2))

        print('-' * 20, 'phase 06 - generate parser code')
        gencode6 = codegen(parser)
        with open('./tmp/g06.py', 'w') as f:
            f.write(gencode6)

        print('-' * 20, 'phase 07 - import generated code')
        py_compile.compile('./tmp/g06.py', doraise=True)
        # g06 = __import__('g06')
        # GenParser = g06.EBNFBootstrapParser

        # print('-' * 20, 'phase 08 - compile using generated code')
        # parser = GenParser(trace=False)
        # result = parser.parse(
        #     text,
        #     'start',
        #     comments_re=COMMENTS_RE,
        #     eol_comments_re=EOL_COMMENTS_RE
        # )
        # self.assertEqual(result, parser.ast['start'])
        # ast8 = parser.ast['start']
        # json8 = json.dumps(asjson(ast8), indent=2)
        # open('./tmp/08.ast', 'w').write(json8)
        # self.assertEqual(ast5, ast8)

        print('-' * 20, 'phase 09 - Generate parser with semantics')
        with open('grammar/tatsu.ebnf') as f:
            text = f.read()
        parser = GrammarGenerator('EBNFBootstrap')
        g9 = parser.parse(text)
        generated_grammar9 = str(g9)
        with open('./tmp/09.ebnf', 'w') as f:
            f.write(generated_grammar9)
        self.assertEqual(generated_grammar9, generated_grammar1)

        print('-' * 20, 'phase 10 - Parse with a model using a semantics')
        g10 = g9.parse(
            text,
            start='start',
            semantics=EBNFGrammarSemantics('EBNFBootstrap')
        )
        generated_grammar10 = str(g10)
        with open('./tmp/10.ebnf', 'w') as f:
            f.write(generated_grammar10)
        gencode10 = codegen(g10)
        with open('./tmp/g10.py', 'w') as f:
            f.write(gencode10)

        print('-' * 20, 'phase 11 - Pickle the model and try again.')
        with open('./tmp/11.tatsu', 'wb') as f:
            pickle.dump(g10, f, protocol=2)
        with open('./tmp/11.tatsu', 'rb') as f:
            g11 = pickle.load(f)
        with open('./tmp/11.ebnf', 'w') as f:
            f.write(str(g11))
        r11 = g11.parse(
            text,
            start='start',
            semantics=EBNFGrammarSemantics('EBNFBootstrap')
        )
        gencode11 = codegen(r11)
        with open('./tmp/bootstrap_g11.py', 'w') as f:
            f.write(gencode11)

        print('-' * 20, 'phase 12 - Walker')

        class PrintNameWalker(DepthFirstWalker):
            def __init__(self):
                self.walked = []

            def walk_default(self, o, children):
                self.walked.append(o.__class__.__name__)

        v = PrintNameWalker()
        v.walk(g11)
        with open('./tmp/12.txt', 'w') as f:
            f.write('\n'.join(v.walked))

        # note: pygraphviz not yet updated
        if sys.version_info >= (3, 7):
            return

        print('-' * 20, 'phase 13 - Graphics')
        try:
            from tatsu.diagrams import draw
        except ImportError:
            print('PyGraphViz not found!')
        else:
            if not util.PY37:
                draw('./tmp/13.png', g11)


def suite():
    return unittest.TestLoader().loadTestsFromTestCase(BootstrapTests)


def main():
    unittest.TextTestRunner(verbosity=2).run(suite())


if __name__ == '__main__':
    main()
