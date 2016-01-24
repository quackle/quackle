# coding: utf-8

import quackle

def startUp(lexicon='twl06',
            alphabet='english',
            datadir='../../data'):

    # Set up the data manager
    dm = quackle.DataManager()
    dm.setComputerPlayers(quackle.ComputerPlayerCollection.fullCollection())
    dm.setBackupLexicon(lexicon)
    dm.setAppDataDirectory(datadir)

    # Set up the alphabet
    abc = quackle.AlphabetParameters.findAlphabetFile(alphabet)
    abc2 = quackle.Util.stdStringToQString(abc) #convert to qstring
    fa = quackle.FlexibleAlphabetParameters()

    assert fa.load(abc2)
    dm.setAlphabetParameters(fa)

    # Set up the board
    board = quackle.BoardParameters()
    dm.setBoardParameters(board)

    # Find the lexicon
    dawg = quackle.LexiconParameters.findDictionaryFile(lexicon + '.dawg')
    gaddag = quackle.LexiconParameters.findDictionaryFile(lexicon + '.gaddag')
    dm.lexiconParameters().loadDawg(dawg)
    dm.lexiconParameters().loadGaddag(gaddag)

    dm.strategyParameters().initialize(lexicon)
    return dm


def getComputerPlayer(dm, name='Speedy Player'):
    player, found = dm.computerPlayers().playerForName(name)
    assert found
    player = player.computerPlayer()
    return player


dm = startUp()

# Create a computer player
player1 = getComputerPlayer(dm)
print player1.name()

# Create the Game file (.gcg) reader
gamereader = quackle.GCGIO()
gamePath = quackle.Util.stdStringToQString('../../test/positions/short_game_with_bad_moves.gcg')
game = gamereader.read(gamePath, quackle.Logania.MaintainBoardPreparation)

# Get the current position
position = game.currentPosition()

player1.setPosition(position)

racks = quackle.ProbableRackList()
unseenbag = position.unseenBag()
if unseenbag.size() <= dm.parameters().rackSize() + 3:
    enum = quackle.Enumerator(unseenbag)
    enum.enumerate(racks)
    for rack in racks:
        print rack

movesToShow = 10

print "Board state: \n%s" % position.board().toString()
print "Move made: %s" % position.moveMade().toString()
print "Current player: %s" % position.currentPlayer().storeInformationToString()
print "Turn number: %i" % position.turnNumber()

movelist = player1.moves(10)

# Show 10 moves suggested by computer player
for move in movelist: print move.toString()
