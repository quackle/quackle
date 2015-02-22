import io

def filterLeaves():
    '''from the default output from Quackle, filter the leave value list
    into "RACK VALUE\n"'''

    leaves = io.open("input/leaves.txt", encoding='utf-8').read().splitlines()
    leavesOutput = io.open("output/leaves.txt", "w", encoding='utf-8')

    for line in leaves:
        ln = line.split()
        if len(ln) == 12:
            leavesOutput.write(u'%s %s\n' % (ln[0], ln[8][:-1]))

    leavesOutput.close()

filterLeaves()
