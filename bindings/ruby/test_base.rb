def initialize_dm(lexicon: 'twl06', alphabet: 'english', datadir: '../../data')
  # Set up the data manager
  dm = Quackle::DataManager.new
  dm.setComputerPlayers(Quackle::ComputerPlayerCollection.fullCollection)
  dm.setBackupLexicon(lexicon)
  dm.setAppDataDirectory(datadir)

  # Set up the alphabet
  abc = Quackle::AlphabetParameters.findAlphabetFile(alphabet)
  abc2 = Quackle::Util.stdStringToQString(abc) #convert to qstring
  fa = Quackle::FlexibleAlphabetParameters.new

  fail unless fa.load(abc2)
  dm.setAlphabetParameters(fa)

  # Set up the board
  board = Quackle::BoardParameters.new
  dm.setBoardParameters(board)

  # Find the lexicon
  dawg = Quackle::LexiconParameters.findDictionaryFile(lexicon + '.dawg')
  gaddag = Quackle::LexiconParameters.findDictionaryFile(lexicon + '.gaddag')
  dm.lexiconParameters.loadDawg(dawg)
  dm.lexiconParameters.loadGaddag(gaddag)

  dm.strategyParameters.init(lexicon)
  return dm
end

def get_computer_player(dm, name: 'Speedy Player')
  player, found = dm.computerPlayers.playerForName(name)
  fail unless found
  player.computerPlayer
end

