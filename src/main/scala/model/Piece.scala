package model

enum Color:
    case White, Black

enum PieceType:
    case Pawn, Knight, Bishop, Rook, Queen, King

case class Piece(color: Color, pieceType: PieceType)