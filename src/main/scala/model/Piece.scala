package model

enum Color:
  case White, Black

  def opposite: Color = this match
    case White => Black
    case Black => White

enum PieceType:
  case Pawn, Knight, Bishop, Rook, Queen, King

case class Piece(color: Color, pieceType: PieceType)