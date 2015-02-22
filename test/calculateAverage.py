#-*- coding: utf-8 -*-

import numpy as np
import io

def calculateAverage():
    '''real values of leaves should be averaged over all possible leaves'''
    
    # I don't really know how this conv works, but I'll keep it for now as I've already spent too much time on this script. Got it from here: http://stackoverflow.com/questions/28454584/unicode-string-in-matplotlib-annotate
    conv = {0:(lambda s: s.decode('utf-8'))}

    values = np.loadtxt("input/leaves.txt", usecols=(1,))
    leaves = np.loadtxt("input/leaves.txt", dtype='unicode', converters=conv, usecols=(0,))

    newValues =  values - np.mean(values)

    outputFile = io.open("output/leaves.txt", 'w')

    for i, elem in enumerate(leaves):
        elem
        outputFile.write(u'%s %f\n' % (leaves[i], newValues[i]))

    outputFile.close()

calculateAverage()
