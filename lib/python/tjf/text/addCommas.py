# -*-coding: utf-8-*- ; -*-Python-*-

#         Copyright © 2022-2022 Tom Fontaine

# Title:  addCommas.py
# Date:   21-Apr-2022


def addCommas(number):
    """Returns NUMBER as string with appropriate commas."""
    if type(number) != str:
        number = str(number)

    if len(number) < 5:
        return number

    reversed = number[::-1]
    numlist  = list(map(lambda idx: reversed[idx:idx+3], range(0, len(number), 3)))

    return ','.join(numlist)[::-1]
