require_relative 'quackle'

# Some higher-level functions over the quackle bindings.

module Quackle
  PLAYER_TYPES = {
    speedy: "Speedy Player",
    championship: "Championship Player",
    championship_20s: "Twenty Second Championship Player",
    championship_5m: "Five Minute Championship Player",
  }

  module GCGUtils
    def self.load_game(path)
      gamereader = GCGIO.new
      gamePath = Util.stdStringToQString(path)
      gamereader.read(gamePath, Logania::MaintainBoardPreparation)
    end
  end

  module BoardUtils
    SYMBOLS = {
      '~' => :w4, '=' => :w3, '-' => :w2,
      '^' => :l4, '"' => :l3, "'" => :l2
    }

    def self.make_board_params(ary)
      # Assume ary is a well-formed rectangular array
      h = ary.length
      w = ary[0].length
      b = BoardParameters.new()
      b.setHeight(h)
      b.setWidth(w)
      ary.each_with_index do |row, y|
        row.each_with_index do |cell, x|
          type, mul = cell.to_s.split(//)
          mul = mul.to_i
          if type == 'w'
            b.setWordMultiplier(y, x, mul)
          elsif type == 'l'
            b.setLetterMultiplier(y, x, mul)
          end
        end
      end
      return b
    end

    def self.read_board_params(str, piped: false, doublespaced: false)
      # Assume str is a board with one row per line.
      # Set doublespaced: true if cells are separated by
      # a space (as in quackle's board display, e.g.)
      # Set piped: true if rows are marked by a | on either side.
      rows = str.lines.map(&:chomp)
      if piped
        rows = rows.select {|r| r.include?('|')}
        rows = rows.map {|r| r.gsub(/^.*?[|](.*?)[|].*$/, '\1')}
      end
      cells = rows.map do |r|
        row = r.split(//)
        if doublespaced
          row = row.each_slice(2).map(&:first)
        end
        row.map {|c| SYMBOLS[c] || :l1}
      end
      make_board_params(cells)
    end
  end

  module RunnerUtils
    def initialize_dm(lexicon:, alphabet:, datadir:, board:nil)
      # Set up the data manager
      dm = DataManager.new
      dm.setComputerPlayers(ComputerPlayerCollection.fullCollection)
      dm.setBackupLexicon(lexicon)
      dm.setAppDataDirectory(datadir)

      # Set up the alphabet
      abc = AlphabetParameters.findAlphabetFile(alphabet)
      abc2 = Util.stdStringToQString(abc) #convert to qstring
      fa = FlexibleAlphabetParameters.new

      fail unless fa.load(abc2)
      dm.setAlphabetParameters(fa)

      # Set up the board
      board ||= BoardParameters.new
      dm.setBoardParameters(board)

      # Find the lexicon
      dawg = LexiconParameters.findDictionaryFile(lexicon + '.dawg')
      gaddag = LexiconParameters.findDictionaryFile(lexicon + '.gaddag')
      dm.lexiconParameters.loadDawg(dawg)
      dm.lexiconParameters.loadGaddag(gaddag)

      dm.strategyParameters.init(lexicon)
      return dm
    end

    def computer_player(type)
      name = PLAYER_TYPES[type]
      if name
        player, found = @dm.computerPlayers.playerForName(name)
        if not found
          raise NameError("Could not initialize computer player #{name}")
        end
        player.computerPlayer
      else
        raise ArgumentError("Unknown computer player: #{type}")
      end
    end
  end

  class AIRunner
    include RunnerUtils

    attr_accessor :dm, :game, :players

    def initialize(lexicon:, alphabet:, datadir:, board: nil, random_seed: nil)
      @dm = initialize_dm(
        lexicon: lexicon, alphabet: alphabet, datadir: datadir, board: board)
      @dm.seedRandomNumbers(random_seed) if random_seed
    end

    def init_game(players)
      @game = Game.new
      @players = PlayerList.new
      players.each_with_index do |(name, type), i|
        player = Player::new(name, Player::ComputerPlayerType, i)
        player.setComputerPlayer(computer_player(type))
        @players << player
      end

      game.setPlayers(@players)
      game.associateKnownComputerPlayers
      game.addPosition
    end

    def current_position
      game.currentPosition
    end

    def current_player
      current_position.currentPlayer
    end

    def board
      current_position.board
    end

    def make_move
      game.haveComputerPlay
    end

    def game_over?
      current_position.gameOver
    end
  end
end
