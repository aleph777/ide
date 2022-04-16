# -*-Python-*-


#         Copyright © 2022-2022 Tom Fontaine

# Title:  randomString.py
# Date:   15-Mar-2022

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


import os
import random

class RandomString:
    def __init__(self, length=16, chars=[], prepend=None, append=None):
        self._length  = length
        self._prepend = prepend
        self._append  = append
        self._chars   = chars

        if len(self._chars) == 0:

            self._chars.extend(self.__charRange__('A', 'Z'))
            self._chars.extend(self.__charRange__('a', 'z'))
            self._chars.extend(self.__charRange__('0', '9'))


    def __charRange__(self, c1, c2, n=1):
        """Return a list of characters between C1 and C2 (by N)."""
        return list(map(lambda x: chr(x), range(ord(c1), ord(c2)+1, n)))


    def set(self, **kwargs):
        """Set private attributes."""
        for key, value in kwargs.items():
            if key == 'length':
                self._length = value
            elif key == 'chars':
                self._chars = value
            elif key == 'prepend':
                self._prepend = value
            elif key == 'append':
                self._append = value


    def get(self):
        retval =  ''.join(list(map(lambda x: self._chars[random.randrange(len(self._chars))], range(self._length))))

        if self._prepend:
            retval = self._prepend + retval

        if self._append:
            retval += self._append

        return retval
