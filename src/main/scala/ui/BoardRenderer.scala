package ui

import model.Board

// The contract every UI honors: given a Board, show it.
trait BoardRenderer:
  def render(board: Board): Unit