# -*-coding: utf-8-*- ; -*-Python-*-

#         Copyright © 2022-2022 Tom Fontaine

# Title:  utils.py|list
# Date:   25-Apr-2022


def flatten(self):
    self.contents = [item for sublist in self.contents for item in sublist]


def insert(lst, offset, text):
    if type(text) == list:
        text.reverse()

        for item in text:
            lst.insert(offset, item)
    else:
        lst.insert(offset, text)


def pop(lst):
    return lst.pop(-1)


def push(lst, text):
    if type(text) == list:
        lst.extend(text)
    else:
        lst.append(text)


def shift(lst):
    return lst.pop(0)


def unshift(lst, text):
    if type(text) == list:
        lst.insert(0, flatten(text))
    else:
        lst.insert(0, text)


def splice(lst, offset, length, text=None):
    tmp = lst[offset:offset+length]

    del lst[offset:offset+length]

    if text:
        insert(lst, offset, text)

    return tmp


def hashFirst(lst, delimiter=','):
    keys = shift(lst)
    keys = keys.split(delimiter)

    nlst = [dict(zip(keys, item.split(delimiter))) for item in lst]

    return keys, nlst


def hashPredefined(lst, keys, delimiter=','):
    nlst = [dict(zip(keys, item.split(delimiter))) for item in lst]

    return nlst


def reverse(lst):
    return lst[::-1]


def transpose(matrix):
    return list(map(list, zip(*matrix)))


# A B C D E      E J O T Y
# F G H I J      D I N S X
# K L M N O  =>  C H M R W
# P Q R S T      B G L Q V
# U V W X Y      A F K P U
def rotateCCW(matrix, rtn=False):
    """Rotate MATRIX counter-clockwise, in-place."""
    n = len(matrix)

    for x in range(n):
        for y in range(n-1, x-1, -1):
            matrix[x][y], matrix[y][x] = matrix[y][x], matrix[x][y]

    matrix.reverse()

    if rtn:
        return matrix


# A B C D E      U P K F A
# F G H I J      V Q L G B
# K L M N O  =>  W R M H C
# P Q R S T      X S N I D
# U V W X Y      Y T O J E
def rotateCW(matrix, rtn=False):
    """Rotate MATRIX clockwise, in-place."""
    n = len(matrix)

    matrix.reverse()

    for x in range(n):
        for y in range(n-1, x-1, -1):
            matrix[x][y], matrix[y][x] = matrix[y][x], matrix[x][y]

    if rtn:
        return matrix
