# coding: utf-8

import quackle

# Set up the data manager
dm = quackle.DataManager()
dm.setComputerPlayers(quackle.ComputerPlayerCollection.fullCollection())
dm.setBackupLexicon('twl06')
dm.setAppDataDirectory('../../data')

# Set up the alphabet
abc = quackle.AlphabetParameters.findAlphabetFile('english')
fa = quackle.FlexibleAlphabetParameters()
assert fa.load(quackle.Util.stdStringToQString(abc))
dm.setAlphabetParameters(fa)

# Set up the board
board = quackle.BoardParameters()
dm.setBoardParameters(board)

# Find the lexicon
dawg = quackle.LexiconParameters.findDictionaryFile('twl06.dawg')
gaddag = quackle.LexiconParameters.findDictionaryFile('twl06.gaddag')
dm.lexiconParameters().loadDawg(dawg)
dm.lexiconParameters().loadGaddag(gaddag)

dm.strategyParameters().initialize('twl06')

# Create a computer player
player1, found = dm.computerPlayers().playerForName('Speedy Player')
assert found
player1 = player1.computerPlayer()
print player1.name()

# Create the Game file (.gcg) reader
gamereader = quackle.GCGIO()
game = gamereader.read(quackle.Util.stdStringToQString('../../test/positions/short_game_with_bad_moves.gcg'),
        quackle.Logania.MaintainBoardPreparation)

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
