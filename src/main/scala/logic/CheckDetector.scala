package logic

import model.*

object CheckDetector:

  // Find where a given color's king sits. Returns None if not found (shouldn't happen in a real game).
  def findKing(board: Board, color: Color): Option[Position] =
    val positions =
      for
        row <- 0 until 8
        col <- 0 until 8
        pos = Position(row, col)
        piece <- board.pieceAt(pos)
        if piece.color == color && piece.pieceType == PieceType.King
      yield pos
    positions.headOption

  // Is `color`'s king currently under attack?
  def isInCheck(board: Board, color: Color): Boolean =
    findKing(board, color) match
      case None => false // no king (shouldn't happen) → treat as not in check
      case Some(kingPos) =>
        // Does ANY enemy piece have a legal move onto the king's square?
        existsEnemyAttacking(board, color, kingPos)

  private def existsEnemyAttacking(board: Board, color: Color, target: Position): Boolean =
    val enemyMovesToTarget =
      for
        row <- 0 until 8
        col <- 0 until 8
        from = Position(row, col)
        piece <- board.pieceAt(from)
        if piece.color != color // enemy piece
        if MoveValidator.isLegal(board, from, target)
      yield from
    enemyMovesToTarget.nonEmpty

      // A move is fully legal if it's geometrically legal AND doesn't leave the mover's own king in check.
  def isLegalConsideringCheck(board: Board, from: Position, to: Position): Boolean =
    MoveValidator.isLegal(board, from, to) && {
      board.pieceAt(from) match
        case None => false
        case Some(piece) =>
          // Simulate the move on a fresh board, then check the mover's king isn't in check.
          val afterMove = board.move(from, to)
          !isInCheck(afterMove, piece.color)
    }

      // Does `color` have at least one fully-legal move available?
  def hasAnyLegalMove(board: Board, color: Color): Boolean =
    val moves =
      for
        fromRow <- 0 until 8
        fromCol <- 0 until 8
        from = Position(fromRow, fromCol)
        if board.pieceAt(from).exists(_.color == color)   // ← filter, not generator
        toRow <- 0 until 8
        toCol <- 0 until 8
        to = Position(toRow, toCol)
        if isLegalConsideringCheck(board, from, to)
      yield (from, to)
    moves.nonEmpty

  // Checkmate: in check, and no legal move to escape it.
  def isCheckmate(board: Board, color: Color): Boolean =
    isInCheck(board, color) && !hasAnyLegalMove(board, color)

  // Stalemate: NOT in check, but no legal move exists (a draw).
  def isStalemate(board: Board, color: Color): Boolean =
    !isInCheck(board, color) && !hasAnyLegalMove(board, color)