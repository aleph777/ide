# -*-coding: utf-8-*- ; -*-Python-*-

#         Copyright © 2022-2022 Tom Fontaine

# Title:  list.py
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


class List:
    def __init__(self, contents):
        self.contents = contents

    def flatten(self):
        self.contents = [item for sublist in self.contents for item in sublist]

    def insert(self, offset, text):

        if type(text) == list:
            text.reverse()

            for item in text:
                self.contents.insert(offset, item)
        else:
            self.contents.insert(offset, text)

    def pop(self):
        return self.contents.pop(-1)

    def push(self, text):
        if type(text) == list:
            self.contents.extend(text)
        else:
            self.contents.append(text)

    def shift(self):
        return self.contents.pop(0)

    def unshift(self, text):
        self.insert(0, text)

        if type(text) == list:
            self.flatten()

    def splice(self, offset, length, text=None):
        tmp = self.contents[offset:offset+length]

        del self.contents[offset:offset+length]

        if text:
            self.insert(offset, text)

        return tmp
