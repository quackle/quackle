require_relative 'quackle_runner'

# Allow a board file to be passed in on the command line
if ARGV[0]
  b = IO.read(ARGV[0])
  board = Quackle::BoardUtils::read_board_params(
    b, doublespaced: true, piped: true)
else
  # use the default empty board
  board = nil
end

runner = Quackle::AIRunner.new(
  lexicon: 'csw15', alphabet: 'english', board: board,
  datadir: '../../data', random_seed: 42)

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
