
import importlib
from pathlib import Path

import pytest  # noqa

from tatsu.exceptions import FailedParse
from tatsu.tool import compile, gencode

INPUT = """
    1d3
"""
OUTPUT = {'number_of_dice': '1', 'sides': '3'}

GRAMMAR = """
    start = expression $;

    int = /-?\d+/ ;

    dice = number_of_dice:factor /d|D/ sides:factor;

    expression = addition ;

    addition
        =
        | left:dice_expr op:('+' | '-') ~ right:addition
        | dice_expr
        ;

    dice_expr
        =
        | dice
        | factor
        ;

    factor
        =
        | '(' ~ @:expression ')'
        | int
        ;
"""


def generate_and_load_parser(name, grammar):
    init_filename = Path('./tmp/__init__')
    init_filename.touch(exist_ok=True)

    parser = gencode(name='Test', grammar=grammar)
    parser_filename = Path(f'./tmp/{name}.py')
    with open(parser_filename, 'wt') as f:
        f.write(parser)
    try:
        importlib.invalidate_caches()
        module = importlib.import_module(f'tmp.{name}', 'tmp')
        importlib.reload(module)
        try:
            return module.UnknownParser()  # noqa
        except (AttributeError, ImportError):
            return module.TestParser()  # noqa
    finally:
        pass
        # parser_filename.unlink()


def test_model_parse():
    model = compile(name='Test', grammar=GRAMMAR)
    assert OUTPUT == model.parse(INPUT)


def test_codegen_parse():
    tmp_dir = Path('./tmp')
    tmp_dir.mkdir(parents=True, exist_ok=True)
    init_filename = Path('./tmp/__init__.py')
    init_filename.touch(exist_ok=True)

    parser = generate_and_load_parser('test_codegen_parse', GRAMMAR)
    output = parser.parse(INPUT, parseinfo=False)
    assert output == OUTPUT


# @pytest.mark.skip('work in progress')
def test_error_messages():
    grammar = '''
        @@grammar :: ORDER
        alphabet = a b others $ ;

        a = 'a' ;
        b = 'b' ;
        others = 'c' | 'd' | 'e' | 'f' |'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o';
    '''
    input = 'a b'

    e1 = None
    model = compile(grammar)
    try:
        model.parse(input)
    except FailedParse as e:  # noqa
        e1 = str(e)
    assert "expecting one of: 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j' 'k' 'l' 'm' 'n' 'o'" in e1


# @pytest.mark.skip('work in progress')
def test_name_checked():
    grammar = '''
        @@grammar :: Test
        @@ignorecase :: True
        @@keyword :: if

        start = rule ;
        rule = @:word if_exp $ ;
        if_exp = 'if' digit ;
        @name word = /\w+/ ;
        digit = /\d/ ;
    '''

    def subtest(parser):
        parser.parse('nonIF if 1', trace=False)
        with pytest.raises(FailedParse):
            parser.parse('if if 1', trace=False)
        with pytest.raises(FailedParse):
            parser.parse('IF if 1', trace=False)

    parser = compile(grammar, 'Test')
    subtest(parser)
    parser = generate_and_load_parser('test_name_checked', grammar)
    subtest(parser)
