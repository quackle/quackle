#-*- coding: utf-8 -*-

import io

def sortRacks(numLetters):
    '''This is useful if we have a list of racks but we are not sure about
    their sorting order - pre-sort them before processing (before calculating
    partial rack probability'''

    leaveFile = io.open("input/racks%d.txt" % numLetters, encoding='utf-8').read().splitlines()
    sortedList = io.open("input/racks%d-sorted.txt" % numLetters, 'w', encoding='utf-8')

    for line in leaveFile:
        ln = line.split()
        sortedList.write(u'%s\n' % (u''.join(sorted(ln[0]))))

for i in range(2,7):
    sortRacks(i)
