# -*-coding: utf-8-*- ; -*-Python-*-

#         Copyright © 2022-2022 Tom Fontaine

# Title:  color.py
# Date:   29-Apr-2022

from math import sqrt


def cleanRGB(value):
    if type(value) == int:
        retval = '{:x}'.format(value)
    elif value[0] == '#':
        retval = value[1:]
    else:
        retval = value

    retval = int(retval, 16)

    return retval


def rgb(value):
    rgb = cleanRGB(value)

    red   = rgb & 0xff0000
    green = rgb & 0x00ff00
    blue  = rgb & 0x0000ff

    red   >>= 16
    green >>= 8

    return red, green, blue


def value(red, green, blue):
    return '#{:02x}{:02x}{:02x}'.format(red, green, blue)


def rgb2hsl(value=None, red=None, green=None, blue=None):
    if value:
        red, green, blue = rgb(value)
    else:
        red   = cleanRGB(red)
        green = cleanRGB(green)
        blue  = cleanRGB(blue)

    color = {'red': red, 'green': green, 'blue': blue}
    keys  = sorted(color.keys(), key=lambda x: color[x])[::-1]

    maxColor = color[keys[0]]
    minColor = color[keys[-1]]
    range    = maxColor - minColor

    l = int((maxColor + minColor)/2 + 0.5)

    if(range < 0.0001):
        s = 0
        h = 0
    else:
        s = int(range/maxColor)

        if keys[0] == 'red':
            h = int(60*(green - blue)/range + 0.5)
        elif keys[0] == 'green':
            h = int(60*(2.0 + (blue - red)/range) + 0.5)
        else:
            h = int(60*(4.0 + (red - green)/range) + 0.5)

    return h, s, l


def hsl2rgb(hue, sat, lum):
    c = (1 - abs(2*lum - 1))*sat
    x = c*(1 - abs((hue/60) % 2 - 1))

    m = lum - c/2

    if 0 <= hue < 60:
        r, g, b = c, x, 0
    elif 60 <= hue < 120:
        r, g, b = x, c, 0
    elif 120 <= hue < 180:
        r, g, b = 0, c, x
    elif 180 <= hue < 240:
        r, g, b = 0, x, c
    elif 240 <= hue < 300:
        r, g, b = x, 0, c
    elif 300 <= hue < 360:
        r, g, b = c, 0, x

    r = (r + m)*255
    g = (g + m)*255
    b = (b + m)*255

    return r, g, b


def distance(value1, value2):
    r1, g1, b1 = rgb(value1)
    r2, g2, b2 = rgb(value2)

    return sqrt(10)*((r1 - r2)**2 +
                     (g1 - g2)**2 +
                     (b1 - b2)**2)


def luminance(value):
    r, g, b = rgb(value)

    r /= 255
    g /= 255
    b /= 255

    r, g, b = map(lambda x: x/12.92 if x <= 0.03928 else ((x + 0.055)/1.055)**2.4, list(r, g, b))

    return 0.2126*r + 0.7152*g + 0.0722*b


def contrast(value1, value2):
    ratio = (luminance(value1) + 0.5)/(luminance(value2) + 0.5)

    if ratio > 1:
        ratio = 1/ratio

    return ratio
