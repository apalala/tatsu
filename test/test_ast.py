# -*- coding: utf-8 -*-
from __future__ import generator_stop

import unittest

from tatsu.ast import AST


class ASTTests(unittest.TestCase):
    def test_ast(self):
        ast = AST()
        self.assertEqual([], list(ast.items()))
        self.assertTrue(hasattr(ast, '__json__'))

    def test_init(self):
        ast = AST()
        data = list(reversed(
            [(0, 0), (1, 2), (2, 4), (3, 6), (4, 8), (5, 10)]
        ))
        for k, v in data:
            ast[k] = v
        self.assertEqual(data, list(ast.items()))

    def test_empty(self):
        ast = AST()
        self.assertIsNone(ast.name)

    def test_add(self):
        ast = AST()
        ast['name'] = 'hello'
        self.assertIsNotNone(ast.name)
        self.assertEqual('hello', ast.name)

        ast['name'] = 'world'
        self.assertEqual(['hello', 'world'], ast.name)

        ast['value'] = 1
        self.assertEqual(1, ast.value)

    def test_iter(self):
        ast = AST()
        ast['name'] = 'hello'
        ast['name'] = 'world'
        ast['value'] = 1
        self.assertEqual(['name', 'value'], list(ast))
        self.assertEqual([['hello', 'world'], 1], list(ast.values()))


def suite():
    return unittest.TestLoader().loadTestsFromTestCase(ASTTests)


def main():
    unittest.TextTestRunner(verbosity=2).run(suite())


if __name__ == '__main__':
    main()
