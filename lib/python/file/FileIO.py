# -*-Python-*-

#         Copyright © 2022-2022 Tom Fontaine

# Title:  FileIO.py
# Date:   09-Mar-2022

# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software",
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.

# Except as contained in this notice, the name(s of the above copyright
# holders shall not be used in advertising or otherwise to promote the sale,
# use or other dealings in this Software without prior written authorization.

# The software is provided "As Is", without warranty of any kind, express or
# implied, including but not limited to the warranties of merchantability,
# fitness for a particular purpose and noninfringement. In no event shall
# the authors or copyright holders be liable for any claim, damages or other
# liability, whether in an action of contract, tort or otherwise, arising
# from, out of or in connection with the software or the use or other
# dealings in the software.

#
# Purpose:  File object
#
# Revision:
#
import sys

from listobj import List as lo

class FileIO(lo.List):
    def __init__(self,
                 contents  = None,
                 path      = None,
                 basename  = 'untitled.txt',
                 basedir   = './',
                 strip     = False,
                 newline   = False,
                 encoding  = 'utf-8',
                 delimiter = None):

        super().__init__(contents)

        self.contents  = contents
        self._path     = path
        self._basename = basename
        self._basedir  = basedir
        self.strip     = strip
        self.newline   = newline
        self.encoding  = encoding
        self.delimiter = delimiter

        if self._path == None:
            if basedir[-1] != '/':
               basedir += '/'
            self._path = basedir + basename

        if contents == None:
            self.contents = []

        self._fh = None

        print('OOPS')


    def __open__(self, mode=None):
        """"""
        if self._path and self._path != '-':
            self._fh = open(self._path, encoding = self.encoding, mode=mode)
        else:
            self._fh = sys.stdout


    def __put__(self):
        """"""
        if self.delimiter:
            if type(self.contents[0]) == list:
                self.contents = [self.delimiter.join(line) for line in self.contents]
            else:
                self.contents = [self.delimiter.join(self.contents)]

        if self.newline:
            self.contents = list(map(lambda x: '{:s}\n'.format(x), self.contents))

        with self._fh as f:
            f.writelines(self.contents)


    def set__(self, **kwargs):
        global __me__

        for key, value in kwargs.items():
            if key == 'path':
                self._path = value


    def changePath(self, path):
        """"""
        self._path = path


    def get(self):
        """"""
        self.__open__('r')

        with self._fh as f:
            self.contents = f.readlines()

        if self.strip:
            self.contents = [line.rstrip() for line in self.contents]

        if self.delimiter:
            self.contents = [line.split(self.delimiter) for line in self.contents]


    def put(self):
        """"""
        self.__open__('w')
        self.__put__()


    def append(self, text):
        """"""
        self.__open__('a')

        self.contents.clear()

        if type(text) == str:
            self.contents.append(text)
        elif type(text) == list:
            self.contents = text
        else:
            print('Type {:s} not supported!!!'.format(str(type(text))))

        self.__put__()
