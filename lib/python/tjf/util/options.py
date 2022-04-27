# -*-coding: utf-8-*- ; -*-Python-*-

#         Copyright © 2022-2022 Tom Fontaine

# Title:  options.py
# Date:   19-Apr-2022

import getopt
import sys


class Options:
    """"""

    def __init__(self, shortarg, longarg):
        self.option = dict()

        try:
            arguments, __ = getopt.getopt(sys.argv[1:], shortarg, longarg)

            for currentArgument, currentValue in arguments:
                self.option[currentArgument] = currentValue if currentValue != '' else True

        except getopt.error as err:
            print(str(err))
