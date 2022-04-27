# -*-coding: utf-8-*- ; -*-Python-*-


#         Copyright © 2022-2022 Tom Fontaine

# Title:  shuffle.py
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


import random

class Shuffle:
    def __init__(self, items=[]):
        self._items = items


    def random(self):
        return random.sample(self._items, len(self._items))


    def perfect(self):
        length = int(len(self._items)/2)

        items1 = self._items[:length]
        items2 = self._items[length:]

        self._items.clear()

        for i in range(max(len(items1), len(items2))):
            if len(items1) > 0:
                self._items.append(items1.pop(0))
            if len(items2) > 0:
                self._items.append(items2.pop(0))

        return self._items


    def imperfect(self):
        rng  = int(len(self._items)/10 + 0.5)
        sign = 1 if random.randrange(2) > 0 else -1
        diff = sign*random.randrange(rng)

        length = int(len(self._items)/2) + diff

        rh = self._items[:length]
        lh = self._items[length:]

        self._items.clear()

        while len(rh) > 0 or len(lh) >0:
            if len(rh) == 0:
                self._items.extend(lh)
                break
            elif len(lh) == 0:
                self._items.extend(rh)
                break

            if random.randrange(2) > 0:
                self._items.append(lh.pop(0))
            else:
                self._items.append(rh.pop(0))

        return self._items
