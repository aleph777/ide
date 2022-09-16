# -*-coding: utf-8-*- ; -*-Python-*-

#         Copyright © 2022-2022 Tom Fontaine

# Title:  pdf.py
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

# Purpose:

# Revision:


import copy
import random


class PDF:
    """"""

    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    #
    def __amount__(self, x, total):
        """Return a list with the correct size for X."""
        return [x]*max(int(self._size*self._dictionary[x]/total + 0.5), 1)

    def __flatten__(self, lol):
        """Returns a flattened list."""
        return [item for sublist in lol for item in sublist]

    def __init__(self, dictionary={}, keys=None, size=100):
        self._dictionary = copy.deepcopy(dictionary)
        self._size       = size

        self._keys = copy.deepcopy(keys) if keys else self._dictionary.keys()

        total = sum(self._dictionary.values())

        self._pdf = self.__flatten__(list(map(lambda x: self.__amount__(x, total), self._keys)))

    def __str__(self):
        return ','.join(self._pdf)

    def get(self):
        return self._pdf[random.randrange(len(self._pdf))]

    def show(self):
        print(self.__str__)
