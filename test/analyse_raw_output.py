# -*- coding: utf-8 -*-
import codecs
import operator
from itertools import chain

def analyse_raw_output(filename='output/playability-reps50000-seed',
                       range1=(1,1+94), range2=(1001,1001+94),
                       range3=(2001,2001+94)):
    """ Analyse the output from quackle's playability mode with alkamid's
    modifications. It outputs two ranked lists: "main" words played and
    hooks.

    Parameters
    ----------
    filename: str
              base filename of output files
    range1,range2,range3: tuple
                            tuples of two numbers - seeds of the simulations. Usually
                            I run 3 simulations in parallel, with seeds starting from 1,
                            1000 and 2000. If you only have one set of sims, just set
                            range2 and range3 to (0,0)
    """
    
    ran1 = range(range1[0], range1[1])
    ran2 = range(range2[0], range2[1])
    ran3 = range(range3[0], range3[1])
    total_range = chain(ran1,ran2,ran3)

    words_dict = {}
    hooks_dict = {}
    
    for i in total_range:
        print('analysing \'{0}{1}\'...'.format(filename, i))
        with open(filename + str(i), encoding='utf-8', mode='r') as f:
            for line in f.readlines():
                sp = line.split()
                if len(sp) > 1 and sp[0] not in ("GAME", "Performance", "Starting", "using"):
                    # hooks are marked by '#'
                    if sp[0][0] == '#':
                        if (sp[0][1] != '(' or sp[0][-1] != ')') and (')' in sp[0]):
                            hooks_dict[sp[0][1:].upper()] = hooks_dict.get(sp[0][1:].upper(), 0) + float(sp[1])
                    else:
                        words_dict[sp[0]] = words_dict.get(sp[0], 0) + float(sp[1])
                    

    sorted_words_dict = sorted(words_dict.items(), key=operator.itemgetter(1), reverse=True)
    sorted_hooks_dict = sorted(hooks_dict.items(), key=operator.itemgetter(1), reverse=True)

    with open("output/output-compact.txt", encoding='utf-8', mode='w') as f, open('output/output-hooks-compact.txt', encoding='utf-8', mode='w') as g:
        for elem in sorted_words_dict:
            f.write('{0}\t{1}\n'.format(elem[0], int(elem[1])))
        for elem in sorted_hooks_dict:
            g.write('{0}\t{1}\n'.format(elem[0], int(elem[1])))

if __name__ == '__main__':
    analyse_raw_output()
