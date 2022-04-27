# -*-coding: utf-8-*- ; -*-Python-*-

#         Copyright © 2022-2022 Tom Fontaine

# Title:  io.py
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


# Purpose:  File object

# Revision: 31-Mar-2022 Fixed bug with delimiter in ‘__put__’
#                       Added ‘__str__’
#           16-Apr-2022 fixed basename/basedir handling
#                       changed put to be non-destructive


import copy
import sys

class File:
    def __init__(self,
                 lines     = None,
                 path      = None,
                 basename  = 'untitled.txt',
                 basedir   = './',
                 strip     = False,
                 newline   = False,
                 encoding  = 'utf-8',
                 delimiter = None):
        self.lines      = lines
        self._path      = path
        self._basename  = basename
        self._basedir   = basedir
        self._strip     = strip
        self._newline   = newline
        self._encoding  = encoding
        self._delimiter = delimiter

        if self._path == None:
            if basedir != None and basename != None:
                if basedir[-1] != '/':
                    basedir += '/'
                self._path = basedir + basename

        if lines == None:
            self.lines = list()

        self._fh = None


    def __open__(self, mode):
        """Open file in MODE."""
        if self._path and self._path != '-':
            self._fh = open(self._path, encoding = self._encoding, mode=mode)
        else:
            self._fh = sys.stdout


    def __put__(self, lines, newline, delimiter):
        """Write lines to file."""
        if delimiter:
            if type(lines[0]) == list:
                output = [delimiter.join(line) for line in lines]
            else:
                output = [delimiter.join(lines)]
        else:
            output = copy.deepcopy(lines)

        if newline:
            output = list(map(lambda x: '{:s}\n'.format(x), output))

        with self._fh as f:
            f.writelines(output)


    def __str__(self):
        copiedLines = copy.deepcopy(self.lines)

        if self._limiter:
            if type(copiedLines[0]) == list:
                copiedLines = [self._delimiter.join(line) for line in copiedLines]
            else:
                copiedLines = [self._delimiter.join(copiedLines)]

        copiedLines = [line.rstrip() for line in copiedLines]

        copiedLines.append('\n')

        return '\n'.join(copiedLines)


    def set(self, **kwargs):
        """Set private attributes."""
        for key, value in kwargs.items():
            if key == 'path':
                self._path = value
            elif key == 'basename':
                self._basename = value
            elif key == 'basedir':
                self._basedir = value
            elif key == 'strip':
                self._strip = value
            elif key == 'newline':
                self._newline = value
            elif key == 'encoding':
                self._encoding = value
            elif key == 'delimiter':
                self._encoding = value

            if self._path == None:
                if self._basedir != None and self._basename != None:
                    if self._basedir[-1] != '/':
                        self._basedir += '/'
                    self._path = self._basedir + self._basename


    def append(self, text, newline=None, delimter=None):
        """Appends TEXT to file."""
        self.__open__('a')

        dl = delimiter if delimiter else self._delimiter
        nl = newline   if newline   else self._newline

        self.__put__(text, nl, dl)


    def get(self, strip=None, delimiter=None):
        """Read file."""
        self.__open__('r')

        dl = delimiter if delimiter else self._delimiter
        st = strip     if strip     else self._strip

        with self._fh as f:
            self.lines = f.readlines()

        if st:
            self.lines = [line.rstrip() for line in self.lines]

        if dl:
            self.lines = [line.split(dl) for line in self.lines]


    def put(self, newline=None, delimiter=None):
        """Write file."""
        self.__open__('w')

        dl = delimiter if delimiter else self._delimiter
        nl = newline   if newline   else self._newline

        self.__put__(self.lines, nl, dl)
