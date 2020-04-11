# coding: utf-8

import time

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

p1 = getComputerPlayer(dm)
p2 = getComputerPlayer(dm)

# Create computer players
player1 = quackle.Player('Compy1', quackle.Player.ComputerPlayerType, 0)
player1.setComputerPlayer(p1)
print player1.name()

player2 = quackle.Player('Compy2', quackle.Player.ComputerPlayerType, 1)
player2.setComputerPlayer(p2)
print player2.name()

dm.seedRandomNumbers(42)

game = quackle.Game()
players = quackle.PlayerList()

players.append(player1)
players.append(player2)

game.setPlayers(players)
game.associateKnownComputerPlayers()
game.addPosition()

for i in range(50):
    if game.currentPosition().gameOver():
        print "GAME OVER"
        break

    player = game.currentPosition().currentPlayer()
    print "Player: " + player.name()
    print "Rack : " + player.rack().toString()

    move = game.haveComputerPlay()
    print 'Move: ' + move.toString()
    print 'Board: \n' + game.currentPosition().board().toString()

    time.sleep(1)
