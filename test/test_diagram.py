# -*- coding: utf-8 -*-
from __future__ import generator_stop

import sys
import unittest
import pytest

from tatsu.tool import compile
from tatsu.util import PY37


class DiagramTests(unittest.TestCase):

    @pytest.mark.skipif(sys.version_info >= (3, 7), reason="pygraphviz not yet updated")
    def test_dot(self):
        grammar = '''
            start = "foo\\nbar" $;
        '''
        try:
            from tatsu.diagrams import draw
        except ImportError:
            return

        m = compile(grammar, 'Diagram')
        if not PY37:
            draw('tmp/diagram.png', m)


def suite():
    return unittest.TestLoader().loadTestsFromTestCase(DiagramTests)


def main():
    unittest.TextTestRunner(verbosity=2).run(suite())


if __name__ == '__main__':
    main()
