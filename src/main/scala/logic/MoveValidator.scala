package logic

import model.*

object MoveValidator:

  // Is moving from `from` to `to` legal on this board?
  // For now: only pawns are validated. Other pieces return false (not yet implemented).
  def isLegal(board: Board, from: Position, to: Position): Boolean =
    board.pieceAt(from) match
      case None => false // nothing to move
      case Some(piece) =>
        piece.pieceType match
          case PieceType.Pawn   => isLegalPawnMove(board, piece, from, to)
          case PieceType.Knight => isLegalKnightMove(board, piece, from, to)
          case _                => false
  private def isLegalPawnMove(
      board: Board,
      pawn: Piece,
      from: Position,
      to: Position
  ): Boolean =
    // Direction: White moves up (row decreases), Black moves down (row increases).
    val direction = pawn.color match
      case Color.White => -1
      case Color.Black => +1

    // Starting row for a two-square move.
    val startRow = pawn.color match
      case Color.White => 6
      case Color.Black => 1

    val rowDelta = to.row - from.row
    val colDelta = to.col - from.col
    val target = board.pieceAt(to)

    // Case 1: one square straight forward, onto an empty square.
    val oneForward =
      colDelta == 0 &&
        rowDelta == direction &&
        target.isEmpty

    // Case 2: two squares forward from the start row, both squares empty.
    val twoForward =
      colDelta == 0 &&
        rowDelta == 2 * direction &&
        from.row == startRow &&
        board.pieceAt(Position(from.row + direction, from.col)).isEmpty &&
        target.isEmpty

    // Case 3: diagonal capture — one forward, one sideways, onto an ENEMY piece.
    val diagonalCapture =
      math.abs(colDelta) == 1 &&
        rowDelta == direction &&
        (target match
          case Some(p) => p.color != pawn.color
          case None    => false
        )

    oneForward || twoForward || diagonalCapture

  // In isLegal's match, change the Knight case:
  //   case PieceType.Knight => isLegalKnightMove(board, piece, from, to)

  private def isLegalKnightMove(
      board: Board,
      knight: Piece,
      from: Position,
      to: Position
  ): Boolean =
    val rowDelta = math.abs(to.row - from.row)
    val colDelta = math.abs(to.col - from.col)

    // An L-shape: 2 in one direction, 1 in the other.
    val isLShape = (rowDelta == 2 && colDelta == 1) || (rowDelta == 1 && colDelta == 2)

    // Destination must not hold a friendly piece.
    val notFriendly = board.pieceAt(to) match
      case Some(p) => p.color != knight.color
      case None    => true

    isLShape && notFriendly
