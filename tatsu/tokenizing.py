# -*- coding: utf-8 -*-
from __future__ import generator_stop


class Tokenizer:
    @property
    def filename(self):
        raise NotImplementedError

    @property
    def ignorecase(self):
        raise NotImplementedError

    @property
    def pos(self):
        raise NotImplementedError

    def goto(self, pos):
        raise NotImplementedError

    def atend(self):
        raise NotImplementedError

    def ateol(self):
        raise NotImplementedError

    def next(self):
        raise NotImplementedError

    def next_token(self):
        raise NotImplementedError

    def match(self, token, ignorecase=False):
        raise NotImplementedError

    def matchre(self, pattern, ignorecase=False):
        raise NotImplementedError

    def posline(self, pos):
        raise NotImplementedError

    def line_info(self, pos=None):
        raise NotImplementedError

    def get_lines(self, start=None, end=None):
        raise NotImplementedError

    def lookahead(self):
        raise NotImplementedError

    def lookahead_pos(self):
        if self.atend():
            return ''
        info = self.line_info()
        return '~%d:%d' % (info.line + 1, info.col + 1)