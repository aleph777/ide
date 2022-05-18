# -*-coding: utf-8-*- ; -*-Python-*-

#         Copyright © 2022-2022 Tom Fontaine

# Title:  color.py
# Date:   29-Apr-2022

from math import sqrt


hue = list()

hue.extend([{'code': 0xff0000, 'name': 'red',             'type': 'red'}]*15)
hue.extend([{'code': 0xff4000, 'name': 'vermilion',       'type': 'orange red'}]*15)
hue.extend([{'code': 0xff8000, 'name': 'orange',          'type': 'orange'}]*15)
hue.extend([{'code': 0xffbf00, 'name': 'golden yellow',   'type': 'khaki'}]*15)
hue.extend([{'code': 0xfff000, 'name': 'yellow',          'type': 'yellow'}]*15)
hue.extend([{'code': 0xbfff00, 'name': 'yellowish green', 'type': 'lime'}]*15)
hue.extend([{'code': 0x80ff00, 'name': 'chartreuse',      'type': 'olive'}]*15)
hue.extend([{'code': 0x40ff00, 'name': 'leaf green',      'type': 'grass green'}]*15)
hue.extend([{'code': 0x00ff00, 'name': 'green',           'type': 'green'}]*15)
hue.extend([{'code': 0x00ff40, 'name': 'cobalt green',    'type': 'bluish green'}]*15)
hue.extend([{'code': 0x00ff80, 'name': 'emerald green',   'type': 'teal'}]*15)
hue.extend([{'code': 0x00ffbf, 'name': 'turquoise green', 'type': 'greenish cyan'}]*15)
hue.extend([{'code': 0x00ffff, 'name': 'turquoise blue',  'type': 'cyan'}]*15)
hue.extend([{'code': 0x00bfff, 'name': 'cerulean blue',   'type': 'bluish cyan'}]*15)
hue.extend([{'code': 0x0080ff, 'name': 'azure',           'type': 'blue'}]*15)
hue.extend([{'code': 0x0040ff, 'name': 'cobalt blue',     'type': 'blue violet'}]*15)
hue.extend([{'code': 0x0000ff, 'name': 'ultramarine',     'type': 'violet'}]*15)
hue.extend([{'code': 0x4000ff, 'name': 'hyacinth',        'type': 'purple violet'}]*15)
hue.extend([{'code': 0x8000ff, 'name': 'violet',          'type': 'purple'}]*15)
hue.extend([{'code': 0xbf00ff, 'name': 'purple',          'type': 'purple magenta'}]*15)
hue.extend([{'code': 0xff00ff, 'name': 'magenta',         'type': 'magenta'}]*15)
hue.extend([{'code': 0xff00bf, 'name': 'reddish purple',  'type': 'crimson'}]*15)
hue.extend([{'code': 0xff0080, 'name': 'ruby red',        'type': 'scarlet'}]*15)
hue.extend([{'code': 0xff0040, 'name': 'carmine',         'type': 'scarlet red'}]*15)


def clean(value):
    if type(value) == int:
        retval = '{:x}'.format(value)
    elif value[0] == '#':
        retval = value[1:]
    else:
        retval = value

    retval = int(retval, 16)

    return retval


def value2rgb(value):
    rgb = clean(value)

    red   = rgb & 0xff0000
    green = rgb & 0x00ff00
    blue  = rgb & 0x0000ff

    red   >>= 16
    green >>= 8

    return red, green, blue


def rgb2value(rgb):
    return '#{:02x}{:02x}{:02x}'.format(*rgb)


def dec2rgb(decimal):
    return tuple(map(lambda x: int(x*255 + 0.5), decimal))


def dec2value(decimal):
    return rgb2value(dec2rgb(decimal))


def rgb2hsl(rgb):
    red, green, blue = tuple(map(lambda x: clean(x)/255, rgb))

    color = {'red': red, 'green': green, 'blue': blue}
    keys  = sorted(color.keys(), key=lambda x: color[x])[::-1]

    maxColor = color[keys[0]]
    minColor = color[keys[-1]]
    rng      = maxColor - minColor

    l = int(500*(maxColor + minColor) + 0.5)/1000

    if rng < 0.0001:
        s = 0
        h = 0
    else:
        s = int(1000*rng/maxColor + 0.5)/1000

        if keys[0] == 'red':
            h = int(60*(green - blue)/rng + 0.5)
        elif keys[0] == 'green':
            h = int(60*(2.0 + (blue - red)/rng) + 0.5)
        else:
            h = int(60*(4.0 + (red - green)/rng) + 0.5)

    return h, s, l


def value2hsl(value):
    return rgb2hsl(value2rgb(value))


def hsl2rgb(hsl):
    h, s, l = hsl

    c = (1 - abs(2*l - 1))*s
    x = c*(1 - abs((h/60) % 2 - 1))

    m = l - c/2

    if 0 <= h < 60:
        r, g, b = c, x, 0
    elif 60 <= h < 120:
        r, g, b = x, c, 0
    elif 120 <= h < 180:
        r, g, b = 0, c, x
    elif 180 <= h < 240:
        r, g, b = 0, x, c
    elif 240 <= h < 300:
        r, g, b = x, 0, c
    elif 300 <= h < 360:
        r, g, b = c, 0, x

    r, g, b = map(lambda x: int((x + m)*255 + 0.5), (r, g, b))

    return r, g, b


def distance(value1, value2):
    r1, g1, b1 = value2rgb(value1)
    r2, g2, b2 = value2rgb(value2)

    return sqrt(10)*((r1 - r2)**2 +
                     (g1 - g2)**2 +
                     (b1 - b2)**2)


def luminance(value):
    r, g, b = map(lambda x: x/255, value2rgb(value))
    r, g, b = map(lambda x: x/12.92 if x <= 0.03928 else ((x + 0.055)/1.055)**2.4, (r, g, b))

    return 0.2126*r + 0.7152*g + 0.0722*b


def contrast(value1, value2):
    ratio = (luminance(value1) + 0.5)/(luminance(value2) + 0.5)

    if ratio > 1:
        ratio = 1/ratio

    return ratio


def shade(value=None, vl=None):
    if value:
        __, __, l = value2hsl(value)
    else:
        l = vl

    if l < 1/16:
        return 'black'
    elif l < 3/16:
        return 'very dark'
    elif l < 5/16:
        return 'dim'
    elif l < 7/16:
        return 'dark'
    elif l < 9/16:
        return 'medium'
    elif l < 11/16:
        return 'light'
    elif l < 13/16:
        return 'very light'
    elif l < 15/16:
        return 'bright'
    else:
        return 'white'


def gray(s):
    return 'gray' if s < 0.1 else ''


def name(value):
    h, s, l = value2hsl(value)

    sh = shade(vl=l)

    if sh == 'black' or sh == 'white':
        hu = ''
        gr = ''
    else:
        hu = hue[int(h)]['name']
        ty = hue[int(h)]['type']
        gr = gray(s)

    cn = ' '.join((sh, gr, hu)) if gr == 'gray' else ' '.join((sh, hu))

    return cn, sh, gr, hu, ty


def invert(value=None, rgb=None):
    if value:
        h, s, l = value2hsl(value)
    else:
        h, s, l = rgb2hsl(rgb)

    h = (h + 180) % 360
    s = abs(0.50 - s)
    l = abs(1.00 - l)

    return rgb2value(hsl2rgb((h, s, l)))
