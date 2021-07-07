import unittest
import warnings


class MiscTests(unittest.TestCase):
    # test that Mapping is imported from collections.abc
    # required in python 3.8.0+
    def test_import_mapping_from_collectionsABC(self):
        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            from tatsu.grammars import Mapping  # noqa

            self.assertEqual(len(w), 0)
