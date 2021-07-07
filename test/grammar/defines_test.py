from __future__ import annotations

import pytest  # noqa

from tatsu.tool import compile, gencode


def test_name_in_option():
    grammar = '''
        start = expr_range ;

        expr_range =
            | [from: expr] '..' [to: expr]
            | expr
            ;

        expr =
            /[\d]+/
        ;
    '''

    model = compile(grammar)

    ast = model.parse('1 .. 10')
    assert ast == {'from': '1', 'to': '10'}

    ast = model.parse('10')
    assert ast == '10'

    ast = model.parse(' .. 10')
    assert ast == {'from': None, 'to': '10'}

    ast = model.parse('1 .. ')
    assert ast == {'from': '1', 'to': None}

    ast = model.parse(' .. ')
    assert ast == {'from': None, 'to': None}

    code = gencode(grammar=grammar)
    print(code)


def test_by_option():
    grammar = '''
        start = expr_range ;

        expr_range =
            | [from: expr] '..' [to: expr]
            | left:expr ','  [right:expr]
            ;

        expr =
            /[\d]+/
        ;
    '''

    model = compile(grammar)

    ast = model.parse(' .. 10')
    assert ast == {'from': None, 'to': '10'}

    ast = model.parse('1, 2')
    assert ast == {'left': '1', 'right': '2'}

    ast = model.parse('1, ')
    assert ast == {'left': '1', 'right': None}


def test_inner_options():
    grammar = '''
        start = switch;
        switch = 'switch' [(on:'on'|off:'off')] ;
    '''

    model = compile(grammar)

    ast = model.parse('switch on')
    assert ast == {'on': 'on', 'off': None}

    ast = model.parse('switch off')
    assert ast == {'off': 'off', 'on': None}

    ast = model.parse('switch')
    assert ast == {'off': None, 'on': None}
