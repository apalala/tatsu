from __future__ import annotations
import importlib
import argparse
from pathlib import Path

from ..tool import gencode
from .import filelist_from_patterns
from .parproc import processing_loop


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


def parallel_test_run(parse, options):

    def pysearch(pattern):
        if pattern.endswith('.py'):
            return pattern
        else:
            if not pattern.endswith('/'):
                pattern += '/'
            if '**' not in pattern:
                pattern += '**/'
            return pattern + '*.py'

    try:
        patterns = [pysearch(p) for p in options.patterns]
        filenames = filelist_from_patterns(
            patterns,
            sizesort=options.sort,
            ignore=options.ignore
        )

        kwargs = vars(options)
        kwargs.pop('patterns', None)
        kwargs.pop('sort', None)
        kwargs.pop('ignore', None)
        parallel = not kwargs.pop('serial', False)

        return processing_loop(parse, filenames, parallel=parallel, **kwargs)

    except KeyboardInterrupt:
        if options.verbose:
            raise


def parse_args():
    argparser = argparse.ArgumentParser(
        prog=__package__,
        # description=DESCRIPTION,
        add_help=False,
    )

    argparser.add_argument(
        'patterns',
        nargs='+',
        metavar='PATTERNS',
        help='filename patterns',
    )
    argparser.add_argument(
        '--help', '-h',
        help='show this help message and exit',
        action='help',
    )
    argparser.add_argument(
        '--ignore', '-i',
        metavar='PATTERN',
        help='ignore these patterns',
        action='append',
    )
    argparser.add_argument(
        '--sort', '-s',
        help='sort files by size',
        action='store_true',
    )
    argparser.add_argument(
        '--serial', '-S',
        help='do not run in parallel',
        action='store_true',
    )
    argparser.add_argument(
        '--trace', '-t',
        help='produce verbose output for a parse',
        action='store_true',
    )
    argparser.add_argument(
        '--verbose', '-v',
        help='output exceptions as they happen',
        action='store_true',
    )
    argparser.add_argument(
        '--exitfirst', '-x',
        help='output exceptions as they happen',
        action='store_true',
    )
    return argparser.parse_args()


def generic_main(parse):
    return parallel_test_run(parse, parse_args())
