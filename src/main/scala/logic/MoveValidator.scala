package logic

import model.*

object MoveValidator:

  // Is moving from `from` to `to` legal on this board?
  def isLegal(board: Board, from: Position, to: Position): Boolean =
    board.pieceAt(from) match
      case None => false // nothing to move
      case Some(piece) =>
        piece.pieceType match
          case PieceType.Pawn   => isLegalPawnMove(board, piece, from, to)
          case PieceType.Knight => isLegalKnightMove(board, piece, from, to)
          case PieceType.Rook   => isLegalRookMove(board, piece, from, to)
          case PieceType.Bishop => isLegalBishopMove(board, piece, from, to)
          case PieceType.Queen  => isLegalQueenMove(board, piece, from, to)
          case PieceType.King   => isLegalKingMove(board, piece, from, to)

  private def isLegalPawnMove(
      board: Board,
      pawn: Piece,
      from: Position,
      to: Position
  ): Boolean =
    val direction = pawn.color match
      case Color.White => -1
      case Color.Black => +1

    val startRow = pawn.color match
      case Color.White => 6
      case Color.Black => 1

    val rowDelta = to.row - from.row
    val colDelta = to.col - from.col
    val target = board.pieceAt(to)

    val oneForward =
      colDelta == 0 && rowDelta == direction && target.isEmpty

    val twoForward =
      colDelta == 0 &&
        rowDelta == 2 * direction &&
        from.row == startRow &&
        board.pieceAt(Position(from.row + direction, from.col)).isEmpty &&
        target.isEmpty

    val diagonalCapture =
      math.abs(colDelta) == 1 &&
        rowDelta == direction &&
        (target match
          case Some(p) => p.color != pawn.color
          case None    => false
        )

    oneForward || twoForward || diagonalCapture

  private def isLegalKnightMove(
      board: Board,
      knight: Piece,
      from: Position,
      to: Position
  ): Boolean =
    val rowDelta = math.abs(to.row - from.row)
    val colDelta = math.abs(to.col - from.col)
    val isLShape = (rowDelta == 2 && colDelta == 1) || (rowDelta == 1 && colDelta == 2)
    isLShape && notFriendly(board, knight, to)

  private def isLegalRookMove(
      board: Board,
      rook: Piece,
      from: Position,
      to: Position
  ): Boolean =
    val straightLine = (from.row == to.row || from.col == to.col) && from != to
    straightLine && pathIsClear(board, from, to) && notFriendly(board, rook, to)

  private def isLegalBishopMove(
      board: Board,
      bishop: Piece,
      from: Position,
      to: Position
  ): Boolean =
    val diagonal =
      math.abs(to.row - from.row) == math.abs(to.col - from.col) && from != to
    diagonal && pathIsClear(board, from, to) && notFriendly(board, bishop, to)

  private def isLegalQueenMove(
      board: Board,
      queen: Piece,
      from: Position,
      to: Position
  ): Boolean =
    isLegalRookMove(board, queen, from, to) ||
      isLegalBishopMove(board, queen, from, to)

  private def isLegalKingMove(
      board: Board,
      king: Piece,
      from: Position,
      to: Position
  ): Boolean =
    val rowDelta = math.abs(to.row - from.row)
    val colDelta = math.abs(to.col - from.col)
    val oneSquare = rowDelta <= 1 && colDelta <= 1 && from != to
    oneSquare && notFriendly(board, king, to)

  // --- shared helpers ---

  // Are all squares strictly between `from` and `to` empty?
  private def pathIsClear(board: Board, from: Position, to: Position): Boolean =
    val rowStep = Integer.signum(to.row - from.row)
    val colStep = Integer.signum(to.col - from.col)
    var current = Position(from.row + rowStep, from.col + colStep)
    var clear = true
    while current != to && clear do
      if board.pieceAt(current).isDefined then clear = false
      else current = Position(current.row + rowStep, current.col + colStep)
    clear

  // Destination must not hold a friendly piece.
  private def notFriendly(board: Board, piece: Piece, to: Position): Boolean =
    board.pieceAt(to) match
      case Some(p) => p.color != piece.color
      case None    => true