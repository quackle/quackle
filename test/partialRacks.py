#-*- coding: utf-8 -*-

import io
import itertools

def loadAlphabet(filename):
    fil = io.open(filename, encoding="utf-8")
    lines = fil.readlines()
    letters = {}
    for line in lines:
        line = line.split()
        if line[0] == 'blank':
            letters[u'?'] = int(line[2])
        else:
            letters[u'%s' % line[0]] = int(line[3])
    fil.close()
    return letters

def adjustBag(rack, bag):
    newBag = bag.copy()
    for letter in rack:
        newBag[letter] -= 1
    return newBag

def partialValue(rackLen):
    
    bag = loadAlphabet("../data/alphabets/polish.quackle_alphabet")
    racks = io.open("racks%d.txt" % rackLen, encoding='utf-8').read().splitlines()
    leaveList = io.open("leavesSorted.txt", encoding='utf-8').read().splitlines()

    leaves = {}

    # create a dictionary of leave:value
    for line in leaveList:
        (lv, val) = line.split()
        leaves[lv] = float(val)

    outFile = io.open("rack%doutput.txt" % rackLen, 'w', encoding='utf-8')

    for r in racks:
        smallerBag = adjustBag(r, bag)
        bagList = []

        for letter in smallerBag:
            bagList += smallerBag[letter]*letter

        it = itertools.combinations(bagList, 7-rackLen)

        sumVals = 0.0
        i = 0.0
        avgVal = 0.0
        
        for elem in it:
            fullrack = u''.join(sorted(r + u''.join(elem)))
            sumVals += leaves[fullrack]
            i+=1.0
        
        avgVal = sumVals / i
        outFile.write(u'%s %f\n' % (r, avgVal))

partialValue(5)
