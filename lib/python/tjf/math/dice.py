# -*-coding: utf-8-*- ; -*-Python-*-

#         Copyright © 2022-2022 Tom Fontaine

# Title:  dice.py
# Date:   20-Apr-2022

import random


class Dice:
    """Roll the dice."""

    def __init__(self, dice=2, sides=6):
        self._dice  = dice
        self._sides = sides
        self.roll   = None

    def throw(self):
        """Just do it."""
        self.roll = list(map(lambda x: random.randrange(1, self._sides+1), range(self._dice)))

        return sum(self.roll)
