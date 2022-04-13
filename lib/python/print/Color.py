# -*-Python-*-


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
    def __init__(self):
        csi = chr(27) + '['

        self._NORMAL  = csi + '0m'
        self._BOLD    = csi + '1m'

        self._BLACK   = csi + '30m'
        self._RED     = csi + '31m'
        self._GREEN   = csi + '32m'
        self._YELLOW  = csi + '33m'
        self._BLUE    = csi + '34m'
        self._MAGENTA = csi + '35m'
        self._CYAN    = csi + '36m'
        self._WHITE   = csi + '37m'


    def __princ__(self, text, bold, end, color):
        colorText = color

        if bold:
            colorText += self._BOLD

        colorText += text + self._NORMAL

        print(colorText, end=end)


    def princ24(self, text, bold=False, end=None, color=0):
        colorText  = chr(27) + '[38;5;' + str(color) + m

        if bold:
            colorText += self._BOLD

        colorText += text + self._NORMAL

        print(colorText, end=end)


    def black(self, text, bold=False, end=None):
        self.__princ__(text, bold, end, self._BLACK)


    def red(self, text, bold=False, end=None):
        self.__princ__(text, bold, end, self._RED)


    def green(self, text, bold=False, end=None):
        self.__princ__(text, bold, end, self._GREEN)


    def yellow(self, text, bold=True, end=None):
        self.__princ__(text, bold, end, self._YELLOW)


    def blue(self, text, bold=False, end=None):
        self.__princ__(text, bold, end, self._BLUE)


    def magenta(self, text, bold=False, end=None):
        self.__princ__(text, bold, end, self._MAGENTA)


    def cyan(self, text, bold=False, end=None):
        self.__princ__(text, bold, end, self._CYAN)


    def white(self, text, bold=False, end=None):
        self.__princ__(text, bold, end, self._WHITE)
