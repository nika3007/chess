package model

case class Board(squares: Vector[Vector[Option[Piece]]])

object Board:
    def empty: Board = Board(Vector.fill(8, 8)(None: Option[Piece]))