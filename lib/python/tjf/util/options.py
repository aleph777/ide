# -*-coding: utf-8-*- ; -*-Python-*-

#         Copyright © 2022-2022 Tom Fontaine

# Title:  options.py
# Date:   19-Apr-2022

import sys

from getopt import error, gnu_getopt


class Options:
    """"""

    def __init__(self, shortarg, longarg):
        self.option = dict()

        try:
            arguments, args = gnu_getopt(sys.argv[1:], shortarg, longarg)

            self.args = args

            for currentArgument, currentValue in arguments:
                self.option[currentArgument] = currentValue if currentValue != '' else True

        except error as err:
            print(str(err))
