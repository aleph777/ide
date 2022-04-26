# -*-coding: utf-8-*- ; -*-Python-*-


#         Copyright © 2022-2022 Tom Fontaine

# Title:  letterFrequency.py
# Date:   30-Mar-2022

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


frequency = {'a': 8.34,
             'b': 1.54,
             'c': 2.73,
             'd': 4.14,
             'e': 12.60,
             'f': 2.03,
             'g': 1.92,
             'h': 6.11,
             'i': 6.71,
             'j': 0.23,
             'k': 0.87,
             'l': 4.24,
             'm': 2.53,
             'n': 6.80,
             'o': 7.70,
             'p': 1.66,
             'q': 0.09,
             'r': 5.68,
             's': 6.11,
             't': 9.37,
             'u': 2.85,
             'v': 1.06,
             'w': 2.34,
             'x': 0.20,
             'y': 2.04,
             'z': 0.06}

upfreq = dict(zip(map(lambda x: x.upper(), frequency.keys()), frequency.values()))
