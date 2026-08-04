package model

case class Board(squares: Vector[Vector[Option[Piece]]])

object Board:
  // Empty board: 8×8 grid, every square None.
  def empty: Board = Board(Vector.fill(8, 8)(None: Option[Piece]))

  // The back-rank order, defined ONCE. Only the color differs between
  // Black (row 0) and White (row 7), so we reuse this and vary the color.
  private val backRankOrder: Vector[PieceType] =
    Vector(
      PieceType.Rook, PieceType.Knight, PieceType.Bishop, PieceType.Queen,
      PieceType.King, PieceType.Bishop, PieceType.Knight, PieceType.Rook
    )

  // Turn the type-order into a full row of that color.
  // .map transforms each PieceType into Some(Piece(color, type)).
  private def backRank(color: Color): Vector[Option[Piece]] =
    backRankOrder.map(pt => Some(Piece(color, pt)))

  // A full row of pawns of one color.
  private def pawnRow(color: Color): Vector[Option[Piece]] =
    Vector.fill(8)(Some(Piece(color, PieceType.Pawn)))

  // An empty row.
  private val emptyRow: Vector[Option[Piece]] =
    Vector.fill(8)(None: Option[Piece])

  // The starting position, assembled row by row (rank 0 = Black back rank).
  def initial: Board = Board(
    Vector(
      backRank(Color.Black),   // row 0
      pawnRow(Color.Black),    // row 1
      emptyRow,                // row 2
      emptyRow,                // row 3
      emptyRow,                // row 4
      emptyRow,                // row 5
      pawnRow(Color.White),    // row 6
      backRank(Color.White)    // row 7
    )
  )