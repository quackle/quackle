require_relative 'quackle'
require_relative 'quackle_runner'

runner = Quackle::AIRunner.new(
  lexicon: 'twl06', alphabet: 'english', datadir: '../../data', random_seed: 42)

# Create a computer player
player1 = runner.computer_player(:speedy)
puts player1.name

# Load the game from a .gcg file
path = '../../test/positions/short_game_with_bad_moves.gcg'
game = Quackle::GCGUtils.load_game(path)

# Get the current position
position = game.currentPosition

player1.setPosition(position)

racks = Quackle::ProbableRackList.new
unseen_bag = position.unseenBag
if unseen_bag.size <= runner.dm.parameters.rackSize + 3
  enum = Quackle::Enumerator.new(unseen_bag)
  enum.enumerate(racks)
  racks.each do |rack|
    puts rack
  end
end

puts "Board state: \n%s" % position.board
puts "Move made: %s" % position.moveMade
puts "Current player: %s" % position.currentPlayer.storeInformationToString
puts "Turn number: %i" % position.turnNumber

movelist = player1.moves(10)

# Show 10 moves suggested by computer player
movelist.each do |move|
  puts move
end
