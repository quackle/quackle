#-*- coding: utf-8 -*-
import io

def sortLeaves(leaveFile):
    '''This is useful if we have a list of leaves but we are not sure about
    their sorting order - pre-sort them before processing (before calculating
    partial rack probability'''

    leaveFile = io.open('input/%s', encoding='utf-8').read().splitlines()
    sortedList = io.open("output/%s-sorted.txt" % leaveFile[:-4], 'w', encoding='utf-8')

    for line in leaveFile:
        ln = line.split()
        sortedList.write(u'%s %s\n' % (u''.join(sorted(ln[0])), ln[1]))

sortLeaves('leaves.txt')
