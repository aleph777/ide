# -*-coding: utf-8-*- ; -*-Python-*-

#         Copyright © 2022-2022 Tom Fontaine

# Title:  charRange.py
# Date:   25-Apr-2022

def charRange(c1, c2, n=1):
    """Return a list of characters between C1 and C2 (by N)."""
    return list(map(lambda x: chr(x), range(ord(c1), ord(c2)+1, n)))
