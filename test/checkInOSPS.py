#-*- coding: utf-8 -*-

import io

def checkInOSPS():
    '''this is a very inefficient way of checking all the words in
    playability list - shouldn't be necessary if simulations work fine'''

    wordlist = io.open("input/wordlist.txt", 'r', encoding='utf-8').read().splitlines()
    toCheck = io.open("input/playability-raw.txt", 'r', encoding='utf-8').read().splitlines()
    output = io.open("output/playability.txt", 'w', encoding='utf-8')

    i = 0
    for line in toCheck:
        ln = line.split()
        if ln[0] in wordlist:
            output.write(ln[0] + u'\t' + ln[1] + u'\n')
        else:
            print ln[0]

    output.close()

checkInOSPS()
