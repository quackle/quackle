require_relative 'quackle_runner'

runner = Quackle::AIRunner.new(
  lexicon: 'twl06', alphabet: 'english', datadir: '../../data', random_seed: 42)

runner.init_game([["Compy1", :speedy],
                  ["Compy2", :speedy]])

while not runner.game_over?
  player = runner.current_player
  puts "Player: #{player.name}"
  puts "Rack : #{player.rack}"

  move = runner.make_move
  puts "Move: #{move}"
  puts "Board: \n #{runner.board}"

  sleep(1)
end
