import io

def stripRacks(numLetters):

    rcks = io.open("input/racks%d" % numLetters, encoding='utf-8').read().splitlines()
    rcks_stripped = io.open("output/racks%d.txt" % numLetters, "w", encoding='utf-8')

    for r in rcks:
        ln = r.split()
        rcks_stripped.write(u'%s\n' % ln[0])

stripRacks(7)

