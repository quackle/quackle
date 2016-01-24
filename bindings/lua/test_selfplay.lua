require 'quackle'

function startUp(lexicon, alphabet, datadir)

    local lexicon = lexicon or 'twl06'
    local alphabet = alphabet or 'english'
    local datadir = datadir or '../../data'

    -- Set up the data manager
    local dm = quackle.DataManager()
    dm:setComputerPlayers(quackle.ComputerPlayerCollection.fullCollection())
    dm:setBackupLexicon(lexicon)
    dm:setAppDataDirectory(datadir)

    -- Set up the alphabet
    local abc = quackle.AlphabetParameters.findAlphabetFile(alphabet)
    local abc2 = quackle.Util.stdStringToQString(abc) --convert to qstring
    local fa = quackle.FlexibleAlphabetParameters()

    assert(fa:load(abc2))
    dm:setAlphabetParameters(fa)

    -- Set up the board
    local board = quackle.BoardParameters()
    dm:setBoardParameters(board)

    -- Find the lexicon
    local dawg = quackle.LexiconParameters.findDictionaryFile(lexicon .. '.dawg')
    local gaddag = quackle.LexiconParameters.findDictionaryFile(lexicon .. '.gaddag')
    dm:lexiconParameters():loadDawg(dawg)
    dm:lexiconParameters():loadGaddag(gaddag)

    dm:strategyParameters():initialize(lexicon)
    return dm

end


function getComputerPlayer(dm, name)

    local name = name or 'Speedy Player'

    local player, found = dm:computerPlayers():playerForName(name)
    assert(found)
    player = player:computerPlayer()

    return player

end

dm = startUp()

p1 = getComputerPlayer(dm)
p2 = getComputerPlayer(dm)

-- Create computer players
player1 = quackle.Player('Compy1', quackle.Player.ComputerPlayerType, 0)
player1:setComputerPlayer(p1)
print(player1:name())

player2 = quackle.Player('Compy2', quackle.Player.ComputerPlayerType, 1)
player2:setComputerPlayer(p2)
print(player2:name())

dm:seedRandomNumbers(42)

game = quackle.Game()
players = quackle.PlayerList()

players:push_back(player1)
players:push_back(player2)

game:setPlayers(players)
game:associateKnownComputerPlayers()
game:addPosition()

for i=1, 50 do
    if game:currentPosition():gameOver() then
        print "GAME OVER"
        break
    end

    player = game:currentPosition():currentPlayer()
    move = game:haveComputerPlay()
    --print("Player: " .. player:name())
    print("Rack : " .. player:rack():toString())
    print('Move: ' .. move:toString())
    print('Board: \n' .. game:currentPosition():board():toString())

end
