# -*-coding: utf-8-*- ; -*-Python-*-

#         Copyright © 2022-2022 Tom Fontaine

# Title:  hexString.py
# Date:   20-Apr-2022


def get(number, delimiter='-', upper=False, lower=False):
    """Returns NUMBER as a delimiter separated string (every 4 digits)."""
    reversed = number[::-1]
    hexlist  = list(map(lambda idx: reversed[idx:idx+4], range(0, len(number), 4)))

    retval = delimiter.join(hexlist)[::-1]

    if upper:
        retval = retval.upper()
    elif lower:
        retval = retval.lower()

    return retval
