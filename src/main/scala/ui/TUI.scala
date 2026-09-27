package ui

import model.*

object TextUI:

  private def symbol(piece: Piece): Char =
    val base = piece.pieceType match
      case PieceType.Pawn   => 'p'
      case PieceType.Knight => 'n'
      case PieceType.Bishop => 'b'
      case PieceType.Rook   => 'r'
      case PieceType.Queen  => 'q'
      case PieceType.King   => 'k'
    piece.color match
      case Color.White => base.toUpper
      case Color.Black => base

  private def squareStr(square: Option[Piece]): Char =
    square match
      case Some(piece) => symbol(piece)
      case None        => '.'

  def render(board: Board): String =
    val rows =
      board.squares.zipWithIndex.map { (row, rowIndex) =>
        val rankLabel = 8 - rowIndex
        val cells = row.map(squareStr).mkString(" ")
        s"$rankLabel $cells"
      }
    val fileLabels = "  a b c d e f g h"
    (rows :+ fileLabels).mkString("\n")

  def print(board: Board): Unit =
    println(render(board))