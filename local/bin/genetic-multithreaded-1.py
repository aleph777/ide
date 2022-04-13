#!/usr/bin/python3

import copy
import os
import random
import sys

import subprocess as sp

def getDistances():
    fname = sys.argv[1] if len(sys.argv) == 2 else os.environ['HOME'] + "/Documents/map4.distances.csv"

    distance = {}

    f = open(fname, 'r')

    while True:
        line = f.readline().strip()

        if not line:
            break

        ttd = line.split(',')

        distance.setdefault(ttd[0], {})

        distance[ttd[0]][ttd[1]] = ttd[2]

    f.close()

    return copy.deepcopy(distance)

def initializePopulation(startIndex, workAmount):
    tmp = genes

    for p in population1:
        p['ROUTE'].append(tmp.pop(0))

        for n in range(1, len(tmp)+1):
            p['ROUTE'].append(tmp.pop(random.randrange(0, len(tmp),1)))

        for r in routeIndex:
            print([r, routeIndex[r], routeIndex[r+1]])

        exit()

        print(p['ROUTE'])
        print(list(map(lambda x: (p['ROUTE'][routeIndex[x]],p['ROUTE'][routeIndex[x+1]]), routeIndex)))
        #print(sum(map(lambda x,y: distance[x][y], map(lambda x: *(p['ROUTE'][x],p['ROUTE'][x+1]), range(len(p['ROUTE']))))))
        exit()

random.seed(int.from_bytes(os.urandom(4), byteorder="big"))

p          = sp.Popen('nproc', stdin=sp.PIPE, stdout=sp.PIPE, stderr=sp.PIPE)
nproc      = int(p.communicate()[0].strip())
genes      = list(map(lambda x: chr(x), range(ord('A'), ord('Z')+1)))
distance   = getDistances()
costf      = '%5d'
maxgen     = 1000
pscale     = 20
population = pscale*len(genes)
nthreads   = nproc
fmt        = ' '.join(["".join(map(lambda x: '%s',genes)),'--',' '.join(map(lambda x: costf, range(1,11)))])
matingPD   = [*range(population)]
routeIndex = [*range(len(genes)), -1]

population1 = []
population2 = []

r1 = []
r2 = []

for n in range(1, population):
    p1 = {'COST': 0, 'ROUTE': copy.deepcopy(r1)}
    p2 = {'COST': 0, 'ROUTE': copy.deepcopy(r2)}

    population1.append(copy.deepcopy(p1))
    population2.append(copy.deepcopy(p2))

initializePopulation(0,1)
