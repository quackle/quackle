#-*- coding: utf-8 -*-

import io
import itertools
import time

def loadAlphabet(filename):
    lines = io.open(filename, encoding="utf-8").read().splitlines()
    
    letters = {}
    for line in lines:
        line = line.split()
        if line[0] == 'blank':
            letters[u'?'] = int(line[2])
        else:
            letters[u'%s' % line[0]] = int(line[3])
    
    return letters

def adjustBag(rack, bag):
    '''take out from the bag the letters that constitue a rack'''

    newBag = bag.copy()
    for letter in rack:
        newBag[letter] -= 1
    return newBag

def partialValue(rackLen):
    '''Calculates the values of partial rack leaves. We need:
    - an alphabet file with letter distributions
    - a list of racks that we want to evaluate
    - a list of evaluated 7-letter racks
    The script evaluates a weighted average of all 7-letter racks
    that can be created using a given partial rack. A better program would
    evaluate them in-play to account for the letters in the bag, but Quackle
    doesn't allow for this.'''
    
    bag = loadAlphabet("../data/alphabets/polish.quackle_alphabet")
    racks = io.open("input/racks%d-sorted.txt" % rackLen, encoding='utf-8').read().splitlines()
    leaveList = io.open("input/leaves-sorted.txt", encoding='utf-8').read().splitlines()

    leaves = {}

    # create a dictionary of leave:value
    for line in leaveList:
        (lv, val) = line.split()
        leaves[lv] = float(val)

    outFile = io.open("output/rack%doutput.txt" % rackLen, 'w', encoding='utf-8')

    for r in racks:
        smallerBag = adjustBag(r, bag)
        bagList = []

        for letter in smallerBag:
            bagList += smallerBag[letter]*letter

        combs= itertools.combinations(bagList, 7-rackLen)
        combSet = set(combs)

        sumVals = 0.0
        i = 0.0
        avgVal = 0.0
        
        for elem in combSet:
            multiplier = 1.0
            for letter in elem:
                multiplier *= smallerBag[letter]
    
            fullrack = u''.join(sorted(r + u''.join(elem)))
            sumVals += leaves[fullrack]*multiplier
            i+= multiplier

        avgVal = sumVals / i
        outFile.write(u'%s %f\n' % (r, avgVal))


for i in range(2,7):
    partialValue(i)
