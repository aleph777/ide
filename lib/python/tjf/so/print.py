# -*-coding: utf-8-*- ; -*-Python-*-

#         Copyright © 2022-2023 Tom Fontaine

# Title:  print.py
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

# Purpose:  Print object for TSP optimizers

# Revision:

class Print:
    def __init__(self, goal):
        self._goal = goal
        self._best = 10**6

        self._NEW_BEST_YES = '😎'
        self._NEW_BEST_NO  = '  '

        self._TEXT_BOLD   = ''.join([chr(27),'[1m'])
        self._TEXT_NORMAL = ''.join([chr(27),'[0m'])
        self._TEXT_RED    = ''.join([chr(27),'[31m'])
        self._TEXT_YELLOW = ''.join([chr(27),'[33m'])
        self._TEXT_GREEN  = ''.join([chr(27),'[32m'])
        self._TEXT_CYAN   = ''.join([chr(27),'[36m'])


    def __colorString__(self, color, string):
        """Returns STRING in COLOR."""
        return ''.join([self._TEXT_BOLD,color,string,self._TEXT_NORMAL])


    def do(self, iteration, population):
        """"""
        minLength = population[0]['LENGTH']

        color   = self._TEXT_GREEN if self._best < self._goal else self._TEXT_RED

        if minLength < self._best:
            self._best = minLength
            ratio      = self.__colorString__(color, '{:4.2f}'.format(self._best/self._goal))
            newBest    = self._NEW_BEST_YES
            color      = self._TEXT_GREEN
        else:
            ratio   = self.__colorString__(color, '{:4.2f}'.format(self._best/self._goal))
            newBest = self._NEW_BEST_NO
            color   = self._TEXT_YELLOW

        print(newBest, end=' ')
        print('{:5d}: {:s}'.format(iteration, ratio), end=' ')
        print(self.__colorString__(color, str(population[0]['LENGTH'])), end=' ')
        print(' '.join(map(lambda x: '{:5d}'.format(x['LENGTH']), population[1:10])), end=' | ')
        print(' '.join(map(lambda x: '{:5d}'.format(x['LENGTH']), population[-10:len(population)])))
