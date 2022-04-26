# -*-coding: utf-8-*- ; -*-Python-*-


#         Copyright © 2022-2022 Tom Fontaine

# Title:  Color.py
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

#
# Purpose: see https://en.wikipedia.org/wiki/ANSI_escape_code
#
# Revision:
#
class Color:
    """Color object"""

    CSI     = chr(27) + '['
    CSI24   = chr(27) + '[38;5;'

    NORMAL  = '0'
    BOLD    = '1'
    BLACK   = '30'
    RED     = '31'
    GREEN   = '32'
    YELLOW  = '33'
    BLUE    = '34'
    MAGENTA = '35'
    CYAN    = '36'
    WHITE   = '37'

    def __init__(self):
        return


    def __makeColor__(self, color):
        """"""
        if type(color) != str:
            color = str(color)

        return Color.CSI + color + 'm'


    def __makeColor24__(self, color):
        """"""
        if type(color) != str:
            color = str(color)

        return Color.CSI24 + color + 'm'


    def __makeText__(self, text, color, bold):
        """"""
        colorText = self.__makeColor__(color)

        if bold:
            colorText += self.__makeColor__(Color.BOLD)

        colorText += text + self.__makeColor__(Color.NORMAL)

        return colorText


    def __makeText24__(self, text, color, bold):
        colorText = self.__makeColor24__(color)

        if bold:
            colorText += self.__makeColor__(Color.BOLD)

        colorText += text + self.__makeColor__(Color.NORMAL)

        return colorText


    def princ(self, text, color, bold, end):
        print(self.__makeText__(color, bold, text), end)


    def princ24(self, text, color=0, bold=False, end=None):
        print(self.__makeText34__(color, bold, text), end)


    def black(self, text, color=None, bold=False):
        return self.__makeText__(text, Color.BLACK, bold)


    def red(self, text, color=None, bold=False):
        return self.__makeText__(text, Color.RED, bold)


    def green(self, text, color=None, bold=False):
        return self.__makeText__(text, Color.GREEN, bold)


    def yellow(self, text, color=None, bold=False):
        return self.__makeText__(text, Color.YELLOW, bold)


    def blue(self, text, color=None, bold=False):
        return self.__makeText__(text, Color.BLUE, bold)


    def magenta(self, text, color=None, bold=False):
        return self.__makeText__(text, Color.MAGENTA, bold)


    def cyan(self, text, color=None, bold=False):
        return self.__makeText__(text, Color.CYAN, bold)


    def white(self, text, color=None, bold=False):
        return self.__makeText__(text, Color.WHITE, bold)
