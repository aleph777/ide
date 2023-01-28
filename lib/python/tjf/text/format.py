# -*-coding: utf-8-*- ; -*-Python-*-

#         Copyright © 2022-2023 Tom Fontaine

# Title:  format.py
# Date:   21-Apr-2022


def format(lines=None, left=None, usekeys=False, keys=None, delimiter=' ', newline=False):
    """"""
    if not lines or type(lines) != list or len(lines) == 0:
        raise Exception("No content!!!")

    if not keys or type(keys) != list or len(keys) == 0:
        raise Exception("Invalid keys!!!")

    if not left or type(left) != list or len(left) == 0:
        raise Exception("Invalid left keys!!!")

    leftd = dict(zip(left, [1]*len(left)))

    length = dict()

    if usekeys:
        for key in keys:
            length[key] = max(len(key), max(map(lambda x: len(x[key]), lines)))
        else:
            for key in keys:
                length[key] = max(map(lambda x: len(x[key]), lines))

    fmt = delimiter.join(map(lambda x: ''.join('{:<', length[x], '}') if x in leftd else ''.join('{:>', length[x], '}'), keys))

    if newline:
        fmt += '\n'

    return fmt
