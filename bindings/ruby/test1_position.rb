require_relative 'quackle'
require_relative 'test_base'

dm = initialize_dm

# Create a computer player
player1 = get_computer_player(dm)
puts player1.name

# Create the Game file (.gcg) reader
gamereader = Quackle::GCGIO.new
gamePath = Quackle::Util.stdStringToQString('../../test/positions/short_game_with_bad_moves.gcg')
game = gamereader.read(gamePath, Quackle::Logania::MaintainBoardPreparation)

# Get the current position
position = game.currentPosition

player1.setPosition(position)

racks = Quackle::ProbableRackList.new
unseenbag = position.unseenBag
if unseenbag.size <= dm.parameters.rackSize + 3
  enum = Quackle::Enumerator.new(unseenbag)
  enum.enumerate(racks)
  racks.each do |rack|
    puts rack
  end
end

movesToShow = 10

puts "Board state: \n%s" % position.board.toString
puts "Move made: %s" % position.moveMade.toString
puts "Current player: %s" % position.currentPlayer.storeInformationToString
puts "Turn number: %i" % position.turnNumber

movelist = player1.moves(10)

# Show 10 moves suggested by computer player
movelist.each do |move|
  puts move.toString
end
