import io

def findNegativeLeaves():
    '''sometimes Quackle incorrectly assigns negative equity to a 7-letter rack.
    This script finds them. Output both the list of negatives and a filtered
    input list without them.'''

    leaves = io.open("input/leaves.txt", encoding='utf-8').read().splitlines()
    leavesNeg = io.open("output/leaves-negative.txt", "w", encoding='utf-8')
    leavesFiltered = io.open("output/leaves.txt", 'w', encoding='utf-8')

    for line in leaves:
        if '-' in line[10:]:
            leavesNeg.write(u'%s\n' % line[:7])
        else:
            leavesFiltered.write(u'%s\n' % line)

    leavesNeg.close()
    leavesFiltered.close()

findNegativeLeaves()
