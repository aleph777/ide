# -*-coding: utf-8-*- ; -*-Python-*-

#         Copyright © 2022-2023 Tom Fontaine

# Title:  statistics.py
# Date:   20-Apr-2022

import functools
import math

from operator import mul


class Statistics:
    """"""

    def product(self, data=None):
        """"""
        if data is None:
            return functools.reduce(mul, self.data)
        else:
            return functools.reduce(mul, data)

    def __init__(self, **kwargs):
        for key, value in kwargs.items():
            if key == 'data':
                self.data  = value
            elif key == 'data2':
                self.data2 = value
            elif key == 'binMinimum':
                binMinimum = value
            elif key == 'binMaximum':
                binMaximum = value
            elif key == 'binWidth':
                binWidth = value
            elif key == 'trim':
                trim = value

        lendata = len(self.data)

        self.maximum  = max(self.data)
        self.minimum  = min(self.data)
        self.maxIndex = self.data.index(self.maximum)
        self.minIndex = self.data.index(self.minimum)

        self.sum   = sum(self.data)
        self.range = self.maximum - self.minimum
        self.mean  = self.sum/lendata

        self.variance = sum(list(map(lambda x: (x - self.mean)**2, self.data)))
        self.stdev    = math.sqrt(self.variance)

        midpt = lendata/2

        self.median = (self.data[midpt-1] + self.data[midpt])/2 if lendata % 2 == 0 else self.data[midpt]

        if trim is not None and (0 <= trim <= 0.45):
            trmin = int(lendata*trim)
            trmax = int(lendata*(1 - trim))
            trrng = trmax - trmin

            if trrng == 0:
                print('not enough data for trimmed mean!!!\n')
                exit()

        self.trimmedMean = sum(self.data[trmin:trmin+trmax])/trrng

        if 0 in self.data:
            self.harmonicMean  = 0
            self.geometricMean = 0
        else:
            self.harmonicMean  = lendata/sum(map(lambda x: 1/x, self.data))
            self.geometricMean = self.product() ** 1/lendata


        if self.data2 is not None and lendata == len(self.data2):
            meanx = self.mean
            meany = sum(self.data2)/lendata

            x = list(map(lambda z: z - meanx, self.data))
            y = list(map(lambda z: z - meany, self.data2))

            xx = sum(list(map(lambda z: z**2, x)))
            yy = sum(list(map(lambda z: z**2, y)))
            xy = sum(list(map(lambda z: x[z]*y[z], range(lendata))))

            self.correlation = xy/math.sqrt(xx*yy)
        else:
            self.correlation = None

        if binMinimum is None or self.minimum < binMinimum:
            binMinimum = int(self.minimum)
        if binMaximum is None or self.maximum > binMaximum:
            binMaximum = int(self.maximum)
        if binWidth is None or binWidth == 0:
            binWidth = 1

        binCount = int((binMaximum - binMinimum)/binWidth) + 1

        self.histogram = [0]*binCount

        for d in self.data:
            self.histogram[int((d - binMinimum)/binWidth)] += 1

        self.mode = max(self.histogram)*binWidth + binMinimum
