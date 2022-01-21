# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
                  rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                  [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                  [[0, 0], [0, -1], [0, 1], [0, 2]]],
                  rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                  rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                  rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                  rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
                  rotations([[0, 0], [-1, 0], [1, 0], [0, -1], [-1, -1]]),
                  [[[0, 0], [-1, 0], [1, 0], [2, 0], [3, 0]],
                  [[0, 0], [0, -1], [0, 1], [0, 2], [0, 3]]],
                  rotations([[0, 0], [-1, 0], [0, -1]])]

  # your enhancements here
  def self.next_piece (board, cheat_active=false)
    if cheat_active 
      MyPiece.new([[[0,0]]], board)
    else
      MyPiece.new(All_My_Pieces.sample, board)
    end
  end
end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    super(game)
    @current_block = MyPiece.next_piece(self)
    @cheat_active = false
  end

  def rotate_180_degrees
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def next_piece 
    if cheat_is_active 
      @current_block = MyPiece.next_piece(self, true)
      @cheat_active = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  # override store_current; store all block locations depending on block size
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0...locations.size).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def cheat_is_active
    @cheat_active
  end

  def cheat
    if score >= 100 && !cheat_is_active
      @cheat_active = true
      @score -= 100 
    end
  end
end

class MyTetris < Tetris
  # your enhancements here
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  # override: add u key binding; rotate 180
  # add c key binding; cheat
  def key_bindings
    @root.bind('u', proc {@board.rotate_180_degrees})
    @root.bind('c', proc {@board.cheat})
    super
  end
end
