# -*-coding: utf-8-*- ; -*-Python-*-

#         Copyright © 2022-2023 Tom Fontaine

# Title:  utils.py
# Date:   25-Apr-2022


import random

from tjf.text.colors import colors


def addCommas(number):
    """Returns NUMBER as string with appropriate commas."""
    if type(number) != str:
        number = str(number)

    if len(number) < 5:
        return number

    reversed = number[::-1]
    numlist  = list(map(lambda idx: reversed[idx:idx+3], range(0, len(number), 3)))

    return ','.join(numlist)[::-1]


def charRange(c1, c2, n=1):
    """Return a list of characters between C1 and C2 (by N)."""
    return list(map(lambda x: chr(x), range(ord(c1), ord(c2)+1, n)))


def hexString(number, delimiter='-', upper=False, lower=False):
    """Returns NUMBER as a delimiter separated string (every 4 digits)."""
    reversed = number[::-1]
    hexlist  = list(map(lambda idx: reversed[idx:idx+4], range(0, len(number), 4)))

    retval = delimiter.join(hexlist)[::-1]

    if upper:
        retval = retval.upper()
    elif lower:
        retval = retval.lower()

    return retval


def randomString(length=16, chars=[], prepend=None, append=None):
    """Returns a random string of LENGTH from CHARS."""
    if len(chars) == 0:
        chars.extend(charRange('A', 'Z'))
        chars.extend(charRange('a', 'z'))
        chars.extend(charRange('0', '9'))

    retval = ''.join(list(map(lambda x: chars[random.randrange(len(chars))], range(length))))

    if prepend:
        retval = prepend + retval

    if append:
        retval += append

    return retval


def reverse(text):
    return text[::-1]


def makeColor(color):
    """"""
    if type(color) != str:
        col = str(color)

    return colors['CSI'] + color + 'm'


def makeColorText(text, color, bold):
    """"""
    colorText = makeColor(color)

    if bold:
        colorText += makeColor(colors['BOLD'])

    colorText += text + makeColor(colors['NORMAL'])

    return colorText


def black(text, bold=False):
    return makeColorText(text, colors['BLACK'], bold)


def red(text, bold=False):
    return makeColorText(text, colors['RED'], bold)


def green(text, bold=False):
    return makeColorText(text, colors['GREEN'], bold)


def yellow(text, bold=False):
    return makeColorText(text, colors['YELLOW'], bold)


def blue(text, bold=False):
    return makeColorText(text, colors['BLUE'], bold)


def magenta(text, bold=False):
    return makeColorText(text, colors['MAGENTA'], bold)


def cyan(text, bold=False):
    return makeColorText(text, colors['CYAN'], bold)


def white(text, bold=False):
    return makeColorText(text, colors['WHITE'], bold)


def testMatrix():
    return [[*charRange('A', 'E')], [*charRange('F', 'J')], [*charRange('K', 'O')], [*charRange('P', 'T')], [*charRange('U', 'Y')]]
