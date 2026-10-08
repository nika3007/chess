package model

case class GameState(board: Board, turn: Color):

  // Produce the next state after a move: new board, turn flips.
  def moveMade(from: Position, to: Position): GameState =
    GameState(board.move(from, to), turn.opposite)

object GameState:
  // A fresh game: starting board, White moves first.
  def initial: GameState = GameState(Board.initial, Color.White)