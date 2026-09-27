package model

case class Board(squares: Vector[Vector[Option[Piece]]]):

  def pieceAt(pos: Position): Option[Piece] =
    squares(pos.row)(pos.col)

  def move(from: Position, to: Position): Board =
    pieceAt(from) match
      case None => this
      case Some(piece) =>
        val cleared = updated(from, None)
        cleared.updated(to, Some(piece))

  private def updated(pos: Position, content: Option[Piece]): Board =
    val newRow = squares(pos.row).updated(pos.col, content)
    Board(squares.updated(pos.row, newRow))

object Board:
  def empty: Board = Board(Vector.fill(8, 8)(None: Option[Piece]))

  private val backRankOrder: Vector[PieceType] =
    Vector(
      PieceType.Rook, PieceType.Knight, PieceType.Bishop, PieceType.Queen,
      PieceType.King, PieceType.Bishop, PieceType.Knight, PieceType.Rook
    )

  private def backRank(color: Color): Vector[Option[Piece]] =
    backRankOrder.map(pt => Some(Piece(color, pt)))

  private def pawnRow(color: Color): Vector[Option[Piece]] =
    Vector.fill(8)(Some(Piece(color, PieceType.Pawn)))

  private val emptyRow: Vector[Option[Piece]] =
    Vector.fill(8)(None: Option[Piece])

  def initial: Board = Board(
    Vector(
      backRank(Color.Black),
      pawnRow(Color.Black),
      emptyRow, emptyRow, emptyRow, emptyRow,
      pawnRow(Color.White),
      backRank(Color.White)
    )
  )