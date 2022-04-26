# -*-coding: utf-8-*- ; -*-Python-*-

#         Copyright © 2022-2022 Tom Fontaine

# Title:  format.py
# Date:   21-Apr-2022


import os
import sys


class Format:
    """"""
    __me__ = os.path.basename(sys.argv[0])

    def __init__(self, lines=None, left=None, usekeys=False, fmtkeys=None, delimiter=',', newline=False):
        if lines is None:
            print('{:s}:{_s}; no content!!!', list(Format.__me__, __name__))

        if left is not None:
            self._left = dict(zip(left, [1]*len(left)))

        length = dict()

        if usekeys:
            for key in fmtkeys:
                length[key] = max(len(key), max(map(lambda x: len(self.lines[x]), self.lines)))
        else:
            for key in fmtkeys:
                length[key] = max(map(lambda x: len(self.lines[x]), self.lines))

        self.format = delimiter.join(map(lambda x: ''.join('{:<', length[x], '}') if x in left else ''.join('{:>', length[x], '}'), fmtkeys))

        if newline:
            self.format += '\n'

        return self.format
