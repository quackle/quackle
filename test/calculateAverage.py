#-*- coding: utf-8 -*-

import numpy as np

def calculateAverage():
    '''real values of leaves should be averaged over all possible leaves'''
    
    values = np.loadtxt("input/leaves.txt", usecols=(1,))
    leaves = np.loadtxt("input/leaves.txt", dtype='str', usecols=(0,))

    values -= np.mean(values)
    outputFile = open("output/leaves.txt", 'w')

    for i, elem in enumerate(leaves):
        outputFile.write('%s %f\n' % (leaves[i], values[i]))

    outputFile.close()

def calculateAverage1():
	#this is SLOWER than calculateAverage() and saves a non-decoded file
    '''real values of leaves should be averaged over all possible leaves'''
    
    values = np.loadtxt("input/leaves.txt", dtype=[('key', 'S8'), ('val', 'f8')])

    values['val'] -= np.mean(values['val'])

    np.savetxt("output/leaves.txt", values, fmt='%s %f')


calculateAverage()
