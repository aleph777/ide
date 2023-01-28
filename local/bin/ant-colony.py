#!/usr/bin/env python3
# -*-Python-*-


#         Copyright © 2022-2023 Tom Fontaine

# Title:  ant-colony.py
# Date:   07-Mar-2022

# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software",
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.

# Except as contained in this notice, the name(s of the above copyright
# holders shall not be used in advertising or otherwise to promote the sale,
# use or other dealings in this Software without prior written authorization.

# The software is provided "As Is", without warranty of any kind, express or
# implied, including but not limited to the warranties of merchantability,
# fitness for a particular purpose and noninfringement. In no event shall
# the authors or copyright holders be liable for any claim, damages or other
# liability, whether in an action of contract, tort or otherwise, arising
# from, out of or in connection with the software or the use or other
# dealings in the software.

#
# Purpose:   optimizes traveling salesman problem using ant colony optimization
#
# Arguments: argv[1] path of graph distances .csv file (optional)
#
# Revision:
#
import copy
import os
import random
import sys
import time

import subprocess as sp

from datetime import datetime
from pathlib  import Path

# from fileio import FileIO as io


__me__ = os.path.basename(sys.argv[0])

if len(sys.argv) == 2 and (sys.argv[1] == '--help' or sys.argv[1] == '-h' or sys.argv[1] == '?'):
    print(''.join(['\nUsage: ', __me__, ' [<path of graph distances .csv file>]\n']))
    exit()

emoji = {
    'NEW_BEST_NO' : {'SYMBOL': '   ', 'TEXT': ''},
    'NEW_BEST_YES': {'SYMBOL': '😎', 'TEXT': ''},
    'CLOWN'       : {'SYMBOL': '🤡', 'TEXT': 'I was told there would be cake.'},
    'MONOCLE'     : {'SYMBOL': '🧐', 'TEXT': 'Nothing to see here...'},
    'NEUTRAL'     : {'SYMBOL': '😐', 'TEXT': 'Boring...'},
    'WORKING_HARD': {'SYMBOL': '😰', 'TEXT': 'Aww, c\'mon already!'},
    'UNAMUSED'    : {'SYMBOL': '😒', 'TEXT': 'Been there, done that.'},
    'ANGUISHED'   : {'SYMBOL': '😧', 'TEXT': 'Are we there yet?'},
    'BANDAGES'    : {'SYMBOL': '🤕', 'TEXT': 'My feet hurt!'},
    'HOT'         : {'SYMBOL': '🥵', 'TEXT': 'Aaargh!'},
    'ANGRY'       : {'SYMBOL': '😠', 'TEXT': 'Seriously?'},
    'DEAD'        : {'SYMBOL': '💀', 'TEXT': 'Enough is enough.'},
    'OOPS'        : {'SYMBOL': '🤪', 'TEXT': ''},
    'THINKING'    : {'SYMBOL': '🤔', 'TEXT': ''},
}

keysStale = ['CLOWN', 'MONOCLE', 'NEUTRAL', 'WORKING_HARD', 'UNAMUSED', 'ANGUISHED', 'BANDAGES', 'HOT', 'ANGRY', 'DEAD']

stalel = ['',*map(lambda x: ': '.join([emoji[x]['SYMBOL'], emoji[x]['TEXT']]), keysStale)]

POPULATION_SIZE  = 50
MAX_ELITE        = 10
MAX_ITERATIONS   = 10**6
ENOUGH_IS_ENOUGH = 5

EVAPORATION_RATE   = 0.95
INITIAL_EXPONENT   = 2
PHEREMONE_EXPONENT = 2
HEURISTIC_EXPONENT = 2

STALE_THRESHOLD = 250
STALE_PARTIAL   = STALE_THRESHOLD/10

TEXT_BOLD   = ''.join([chr(27),'[1m'])
TEXT_NORMAL = ''.join([chr(27),'[0m'])
TEXT_RED    = ''.join([chr(27),'[31m'])
TEXT_YELLOW = TEXT_BOLD + ''.join([chr(27),'[33m'])
TEXT_GREEN  = ''.join([chr(27),'[32m'])
TEXT_CYAN   = ''.join([chr(27),'[36m'])

def charRange(c1, c2, n=1):
    """Return a list of characters between C1 and C2 (by N)."""
    return list(map(lambda x: chr(x), range(ord(c1), ord(c2)+1, n)))


def flatten(l):
    """Returns a flattened list."""
    return [item for sublist in l for item in sublist]


def dict2pdf(d):
    """Creates a PDF array."""
    return flatten(list(map(lambda x: list(map(lambda y: x, range(d[x]))),d.keys())))


def dictFromKeys(keys):
    """Creates a dictionary from KEYS."""
    return dict(zip(keys, list(map(lambda x: 1, keys))))


def dictFromKeyValue(keys, values):
    """Creates a dictionary from a list of KEYS and VALUES."""
    return dict(zip(keys, list(map(lambda x: x, values))))


def nproc():
    """Get the number of available processors."""
    p = sp.Popen('nproc', stdin=sp.PIPE, stdout=sp.PIPE, stderr=sp.PIPE)
    n = int(p.communicate()[0].strip())

    p.terminate()

    return(n)


def getDistances(path):
    """Create a dictionary (of dictionaries) of distance between nodes in the graph."""
    global distance

    fh = open(path, 'r')

    with fh as f:
        lines = f.readlines()

    lines = [line.rstrip() for line in lines]
    lines = [line.split(',') for line in lines]

    distance = {}

    for line in lines:
        distance.setdefault(line[0], {})

        distance[line[0]][line[1]] = int(line[2])

    return distance


def nn():
    """Returns the result of the Nearest Neighbor algorithm."""
    global distance

    route = {'A': 1}
    node1  = 'A'
    node2  = None
    length = 0

    while len(route) < len(distance):
        keys = sorted(distance[node1].keys(), key=lambda x: distance[node1][x])

        for n in range(len(keys)):
            if keys[n] not in route:
                node2 = keys[n]
                break

        route[node2] = 1

        length += distance[node1][node2]

        node1 = node2

    length += distance[node1]['A']

    return length


def initializeHeuristic():
    """Heuristic between A and B = (1/distance{A}{B}**INITIAL_EXPONENT)/sum(distance{A}{B}, distance{A}{C}, ...)."""
    global heuristic, nodes

    for node in nodes:
        keys = list(distance[node].keys())
        rcp2 = list(map(lambda x: 1/distance[node][x]**INITIAL_EXPONENT, keys))
        s    = sum(rcp2)

        heuristic[node] = dict(zip(keys, list(map(lambda x: rcp2[x]/s, range(len(keys))))))


def initializePheremone():
    """Pheremone initialized to 1."""
    global pheremone, nodes

    for node in nodes:
        keys = distance[node].keys()
        r    = dict(zip(keys, list(map(lambda x: 1, keys))))

        r[node] = 0

        pheremone[node] = r


def constructProbabilityDistribution(node, dst):
    """"""
    global heuristic, pheremone

    heu = heuristic[node]
    phe = pheremone[node]

    # P(ij) = τ(ij)**α⋅η(ij)**β/𝜮(τ**α⋅η**β)
    #
    tau_x_beta = dict(zip(dst, map(lambda x: phe[x]**PHEREMONE_EXPONENT*heu[x]**HEURISTIC_EXPONENT, dst)))
    sum_tau_x_beta = sum([tau_x_beta[key] for key in dst])

    normal = dict(zip(dst, list(map(lambda x: max(int(1000*tau_x_beta[x]/sum_tau_x_beta + 0.5), 1), dst))))

    return dict2pdf(normal)


def updateLocalPheremones(route, length):
    """τ′ = φ⋅(1 - ⍴)/L + ⍴⋅τ(ij)"""
    global nodeIdx, pheremone

    for idx in nodeIdx:
        pheremone[route[idx]][route[idx+1]] = 1000*(1 - EVAPORATION_RATE)/length + EVAPORATION_RATE*pheremone[route[idx]][route[idx+1]]


def constructAntSolution(ant):
    """"""
    global nodes, nodeIdx, oops

    node  = nodes[0]
    route = ant['ROUTE']
    pd    = []

    route.clear()
    route.append(node)

    # oopsmin = int(len(nodes)/10 + 0.5)
    oopsmin = 0

    routed = {node: 1}

    for i in range(1, len(nodes)):
        phm = pheremone[node]
        dst = list(filter(lambda x: x not in routed, nodes))

        if len(dst) == 1:
            node = dst[0]

            route.append(node)

            routed[node] = 1

            continue

        if bored and random.randrange(len(nodes)) <= oopsmin:
            pd    = dst
            oops += 1
        else:
            heu = heuristic[node]
            pd  = constructProbabilityDistribution(node, dst)

        idx  = random.randrange(len(pd))
        node = pd[idx]

        route.append(node)

        routed[node] = 1

    ant['LENGTH'] = sum(map(lambda x: distance[route[x]][route[x+1]], nodeIdx))
    ant['TEXT']   = ''.join(route)

    updateLocalPheremones(ant['ROUTE'], ant['LENGTH'])


def constructAntSolutions(iteration):
    """"""
    global ants, oops

    # oops = 0

    for ant in ants:
        constructAntSolution(ant)

        ant['ITERATION'] = iteration

    return sorted(ants, key=lambda x: x['LENGTH'])


def processElite(iteration):
    """"""
    global ants, bestLength, bored, elite, elited, elitel, metGoalIteration, newBest, newElite, printFlag, stale

    tmpd = {}
    tmpl = []

    for ant in ants:
        if ant['TEXT'] not in tmpd:
            if ant['TEXT'] not in elited:
                tmpl.append(ant)
            tmpd[ant['TEXT']] = 1

    elitel = copy.deepcopy(sorted(elitel+tmpl, key=lambda x: x['LENGTH'])[0:MAX_ELITE])

    newElite = ','.join(map(lambda x: str(x['LENGTH']), elitel))

    if newElite != elite:
        metGoalIteration = iteration

        if elitel[0]['LENGTH'] < bestLength:
            newBest    = emoji['NEW_BEST_YES']['SYMBOL']
            bestLength = elitel[0]['LENGTH']
        else:
            newBest    = emoji['NEW_BEST_NO']['SYMBOL']

        for el in elitel:
            length = el['LENGTH']
            text   = el['TEXT']

            if text not in elited:
                elited[text] = length
                el['NEW']    = 0

            el['NEW'] += 1

        printFlag = True
        elite     = newElite

        stale = 0
        bored = False

        return True
    else:
        return False


def processAnts(iteration):
    """"""
    global bored, done, iterationMax, newBest, oops, printFlag, spacer, stale, stalel

    if not processElite(iteration):
        printFlag = False
        newBest   = emoji['NEW_BEST_NO']['SYMBOL']
        bored     = True

        stale += 1

        if stale % STALE_PARTIAL == 0:
            staleIdx  = int(stale/STALE_PARTIAL)
            fmtStalel = stalel[staleIdx].split(': ')
            fmtStale  = '{:2s}{:8d}'.format(fmtStalel[0], iteration)
            spacer    = '\n'

            if oops > 0:
                # print('{:2s}{:8s}: Used uniform PDF {:d} times...'.format(emoji['OOPS']['SYMBOL'], ' ', oops))
                print('{:2s}{:2s}{:5s}: Used uniform PDF {:d} times...'.format(fmtStalel[0], emoji['OOPS']['SYMBOL'], ' ', oops))

                oops = 0
            else:
                print('{:s}: {:s}'.format(fmtStale, fmtStalel[1]))

        if stale > STALE_THRESHOLD:
            iterationMax = iteration
            done         = True


def colorString(color, string):
    """Returns STRING in COLOR."""
    return ''.join([TEXT_BOLD,color,string,TEXT_NORMAL])


def printRoutes(ants):
    """"""
    global newBest, nodeIdx

    for ant in ants:
        route = ant['ROUTE']

        print(' '.join(['NODE: ', *map(lambda x: '{:4s}'.format(x), [*route, 'A'])]))
        print(' '.join(['MILES: 0', *map(lambda x: '{:4d}'.format(distance[route[x]][route[x+1]]), nodeIdx)]))

        total = '0'
        d     = 0

        for x in nodeIdx:
            d     += distance[route[x]][route[x+1]]
            total += '{:5d}'.format(d)

        print(''.join(['TOTAL: ', total, '\n']))

    newBest = emoji['NEW_BEST_NO']['SYMBOL']


def printCurrentIteration(iteration):
    """"""
    global ants, done, elite, elited, elitel, goal, newBest, oops, spacer

    fmtElite = ' '.join(list(map(lambda x: '{:4d}'.format(x['LENGTH']), elitel)))

    tmp = list(filter(lambda x: x['NEW'] == 1, elitel))

    ratio = elitel[0]['LENGTH']/goal
    color = TEXT_GREEN if ratio < 1 else TEXT_RED

    fmtRatio = colorString(color,'{:4.2f}'.format(ratio))

    for ne in tmp:
        length   = str(ne['LENGTH'])
        fmtElite = fmtElite.replace(length, colorString(TEXT_GREEN, length), 10)

    if elitel[0]['NEW'] > 1:
        length   = str(elitel[0]['LENGTH'])
        fmtElite = fmtElite.replace(length, colorString(TEXT_YELLOW, length), 1)

    fmtElite = ' '.join([fmtRatio, fmtElite])

    if not done:
        ants  = sorted(ants, key=lambda x: x['LENGTH'])
        worst = ' '.join(map(lambda x: '{:5d}'.format(ants[x]['LENGTH']), range(-10,0)))

        print('{:2s}{:8d}: {:s} {:4s} {:s}'.format(newBest, iteration+1, fmtElite, '...', worst))
    else:
        print('{:10d} {%d}'.format(iteration, ants[0]['LENGTH']))


def updatePheremones():
    """"""
    global elitel, nodeIdx, pheremone

    taud = {}

    # sum the contributions for each elite ant — τ(ij) = 𝜮(1/L)
    #
    for ant in elitel[0:MAX_ELITE]:
        route = ant['ROUTE']
        tau   = 10000/ant['LENGTH']

        for idx in nodeIdx:
            taud.setdefault(route[idx], {})

            if route[idx+1] not in taud[route[idx]]:
                taud[route[idx]][route[idx+1]] = 0

            taud[route[idx]][route[idx+1]] += tau

    # pheremone(ij) = (1 - ρ)τ(ij) + ρ×𝜮τ
    #
    for node1 in nodes:
        for node2 in nodes:
            if node2 not in taud[node1]:
                continue

            pheremone[node1][node2] = (1 - EVAPORATION_RATE)*taud[node1][node2] + EVAPORATION_RATE*pheremone[node1][node2]


# Begin main()
#
random.seed(int.from_bytes(os.urandom(4), byteorder="big"))

defaultDir  = os.environ['HOME'] + '/Documents/data/'
defaultFile = defaultDir + 'map4.distances.csv'
actualFile  = sys.argv[1] if len(sys.argv) == 2 else defaultFile

pheremone = {}
heuristic = {}
elited    = {}
elitel    = []

antd = {'ROUTE': [], 'LENGTH': None, 'ITERATION': None}
ants = list(map(lambda x: copy.deepcopy(antd), range(POPULATION_SIZE)))

timestamp = datetime.now().strftime("%Y%m%d%H%M%S")
distance  = getDistances(actualFile)
outKeys   = ['ITERATION', *map(lambda x: 'L{:d}'.format(x), range(10)), *map(lambda x: 'R{:d}'.format(x), range(10))]
metaKeys  = ['TIMESTAMP', 'GOAL', 'ITERATIONS', 'POPULATION', 'ELITE', 'EVAPORATION_RATE', 'INITIAL_EXPONENT', 'HEURISTIC_EXPONENT', 'PHEREMONE_EXPONENT' 'STALE_THRESHOLD']

# outFilePath  = '.'.join([Path(actualFile).stem, timestamp, 'output.csv'])
# metaFilePath = '.'.join([Path(actualFile).stem, timestamp, 'metadata.csv'])

# outFile  = io.FileIO(basedir=defaultDir, basename=outFilePath,  newline=True)
# metaFile = io.FileIO(basedir=defaultDir, basename=metaFilePath, newline=True)

# outFile.push(','.join(outKeys))
# outFile.put()

nodes   = sorted(distance.keys())
nodeIdx = [*range(len(nodes)-1), -1]

newBest   = emoji['NEW_BEST_NO']['SYMBOL']
printFlag = True
spacer    = ''
elite     = ''

done  = False
bored = False
stale = 0
oops  = 0

goal         = nn()
bestLength   = goal
iterationMax = 0

print('Nearest Neighbor length: {:d}\n'.format(goal))

time1 = time.time()

initializeHeuristic()
initializePheremone()
constructAntSolutions(0)

processAnts(0)
printCurrentIteration(0)

for iteration in range(1, MAX_ITERATIONS):
    updatePheremones()
    constructAntSolutions(iteration)
    processAnts(iteration)

    if printFlag:
        printCurrentIteration(iteration)

    if done:
        print('')
        printRoutes([elitel[0]])

        duration = time.time() - time1
        minutes  = int(duration/60)
        seconds  = int((duration % 60) + 0.5)

        print('\nduration =',minutes,'minutes, ',seconds,'seconds')
        break
