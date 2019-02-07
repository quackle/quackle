require_relative 'quackle'
require_relative 'test_base'


dm = initialize_dm

p1 = get_computer_player(dm)
p2 = get_computer_player(dm)

# Create computer players
player1 = Quackle::Player.new('Compy1', Quackle::Player::ComputerPlayerType, 0)
player1.setComputerPlayer(p1)
puts player1.name

player2 = Quackle::Player.new('Compy2', Quackle::Player::ComputerPlayerType, 1)
player2.setComputerPlayer(p2)
puts player2.name

dm.seedRandomNumbers(42)

game = Quackle::Game.new
players = Quackle::PlayerList.new

players << player1
players << player2

game.setPlayers(players)
game.associateKnownComputerPlayers
game.addPosition


50.times do
  if game.currentPosition.gameOver
    puts 'GAME OVER'
    break
  end

  player = game.currentPosition.currentPlayer
  puts 'Player: ' + player.name
  puts 'Rack : ' + player.rack.toString

  move = game.haveComputerPlay
  puts 'Move: ' + move.toString
  puts "Board: \n" + game.currentPosition.board.toString

  sleep(1)
end
