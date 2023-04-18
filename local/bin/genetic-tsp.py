#!/usr/bin/env python3
# -*-Python-*-

#         Copyright © 2022-2023  Tom Fontaine

import copy
import os
import random
import sys
import time

import subprocess as sp

def irand(low, high):
    return random.randrange(low, high)

def splice(dest, offset, length, src=[]):
    """Removes the elements designated by OFFSET and LENGTH from an array, and replaces them with the elements of LIST, if any."""
    del dest[offset:offset+length]

    if not src:
        return copy.deepcopy(dest)

    for n in range(len(src)):
        dest.insert(n+offset, src[n])

    return copy.deepcopy(dest)

def getFile(fname):
    """Return the contents of file FNAME as a list of stripped lines."""
    text = []

    with open(fname, encoding = 'utf-8') as f:
            while True:
                line = f.readline().strip()

                if not line:
                    break

                text.append(line)

    return text

def getDistances(text):
    """Create a dictionary (of dictionaries) of distance between nodes in the graph."""
    distance = {}

    # line is in the form of "node1,node2,distance"
    #
    for line in text:
        n1n2d = line.split(',')

        distance.setdefault(n1n2d[0], {})

        distance[n1n2d[0]][n1n2d[1]] = int(n1n2d[2])

    return copy.deepcopy(distance)

def getNproc():
    """Get the number of available processors."""
    p     = sp.Popen('nproc', stdin=sp.PIPE, stdout=sp.PIPE, stderr=sp.PIPE)
    nproc = int(p.communicate()[0].strip())

    p.terminate()

    return(nproc)

def charRange(c1, c2, n=1):
    """Return a list of characters between C1 and C2 (by N)."""
    return list(map(lambda x: chr(x), range(ord(c1), ord(c2)+1, n)))

def computeCost(p):

    return sum(list(map(lambda x: distance[x[0]][x[1]], list(map(lambda x: (p['ROUTE'][routeIndex[x]], p['ROUTE'][routeIndex[x+1]]), routeIndex[0:len(routeIndex)])))))

def initializePopulation(populationSize):
    """Creates the initial population."""
    route      = []
    population = []

    for n in range(populationSize):
        p = {'COST': 0, 'ROUTE': copy.deepcopy(route)}
        r = random.sample(genes[1:len(genes)], len(genes)-1)

        p['ROUTE'].append(genes[0])

        for item in r:
            p['ROUTE'].append(item)

        p['COST'] = computeCost(p)

        population.append(copy.deepcopy(p))

    return copy.deepcopy(population)

def getUnused(distance, unused):
    """"""
    unusedL = list(unused.keys())

    if len(unusedL) == 1:
        return unusedL[0]

    s = sum(list(map(lambda x: distance[x], unusedL)))
    d = list(map(lambda x: [x] * int(100*(1.0 - distance[x]/s)), unusedL))
    d = [item for sublist in d for item in sublist]

    return d[irand(0, len(d)-1)]

def mate(parent1, parent2):
    """Produce a child from the parents"""
    child = {'COST': 0}

    mutationLimit = int(len(parent1['ROUTE'])/2)

    unused = {}
    routeD = {}
    routeL = []
    route1 = copy.deepcopy(parent1['ROUTE'])
    route2 = copy.deepcopy(parent2['ROUTE'])

    for n in range(len(genes)):
        nextR1 = route1.pop(0)
        nextR2 = route2.pop(0)

        if nextR1 in routeD and nextR2 in routeD:
           mutationLimit -= 1

           next = getUnused(distance[routeL[-1]], unused)
        elif nextR1 in routeD:
            next = nextR2
        elif nextR2 in routeD:
            next = nextR1
        elif nextR1 == nextR2:
            next = nextR1
        else:
            d1 = distance[routeL[-1]][nextR1]
            d2 = distance[routeL[-1]][nextR2]

            if random.random() > d1/(d1 + d2):
                next = nextR1

                unused[nextR2] = 1
            else:
                next = nextR2

                unused[nextR1] = 1

            mutationLimit -= 1

        routeL.append(next)

        routeD[next] = 1

        if next in unused:
            del unused[next]

    mutationLength = irand(1,max(mutationLimit,2))
    high           = len(routeL)-mutationLength
    mutationIndex  = irand(1, high)
    mutation       = routeL[mutationIndex:mutationIndex+mutationLength]

    routeL = splice(routeL, mutationIndex, mutationLength)
    routeL = splice(routeL,irand(1,len(routeL)-1),0,mutation)

    child['ROUTE'] = routeL
    child['COST']  = computeCost(child)

    return copy.deepcopy(child)

def getNextPopulation(population):
    """Returns the successor generation."""

    for n in range(len(population)):
        parents = [n, n]

        while parents[0] == n or parents[1] == n:
            parents = random.sample(matingPD, 2)

        child = mate(population[parents[0]], population[parents[1]])

        if child['COST'] < population[n]['COST']:
            population[n] = child

    return sorted(copy.deepcopy(population), key=lambda x: x['COST'])
#
# Begin program
#
defaultFile = os.environ['HOME'] + '/Documents/map4.distances.csv'
actualFile  = sys.argv[1] if len(sys.argv) == 2 else defaultFile
distance    = getDistances(getFile(actualFile))
genes       = charRange('A', 'Z')

random.seed(int.from_bytes(os.urandom(4), byteorder="big"))

costFmt = '{:5d} '
maxgen  = 5000
mindiff = 0
scale   = 20
threads = getNproc()

populationSize = scale*len(genes)
matingPD       = [*range(populationSize)]
display        = [*range(0, populationSize, 50), -1]
routeIndex     = [*range(len(genes)-1), -1]
population     = sorted(initializePopulation(populationSize), key=lambda x: x['COST'])

unused = {}

# fmt1 =  '{0:5d}. '
fmt2 =  ''.join(map(lambda x: '{:s}', genes))
fmt3 = ' '.join(map(lambda x: costFmt, display))

savedText = ''
newBest   = ''
stale     = 0

best  = 10**6
time1 = time.time()

for generation in range(1, maxgen+1):
    text1 = '{0:5d}. '.format(generation)
    text2 = fmt2.format(*map(lambda x: population[0]['ROUTE'][x], range(len(genes))))
    text3 = fmt3.format(*map(lambda x: population[x]['COST'],     display))

    if population[0]['COST'] < best:
        newBest = '😎'
        best    = population[0]['COST']
    else:
        newBest = '  '

    if savedText != text3:
        print(text1, text2, '--', newBest, text3)

    if population[-1]['COST'] - population[0]['COST'] <= mindiff or stale == 1000:
        break

    population = getNextPopulation(population)

    if savedText == text3:
        stale += 1

        if stale % 100 == 0:
            print('  😐')
    else:
        stale = 0

    savedText = text3

duration = time.time() - time1
minutes  = int(duration/60)
seconds  = int((duration % 60) + 0.5)
route    = population[0]['ROUTE']
total    = '0'
d        = 0

for r in routeIndex:
    d += distance[route[r]][route[r+1]]

    total = ''.join([total, ' {:4d}'.format(d)])

print('')
print('NODE: ', ' '.join([*map(lambda x: '{:4s}'.format(x), route),'{:4s}'.format('A')]))
print('MILES:', ' '.join([' ',*map(lambda x: '{:4d}'.format(distance[route[x]][route[x+1]]), routeIndex)]))
print('TOTAL:', total)
print('')
print('duration =',minutes,'minutes, ',seconds,'seconds')
