package ui

import logic.*
import model.*
import scalafx.Includes.*
import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.layout.GridPane
import scalafx.scene.layout.StackPane
import scalafx.scene.paint.Color as FxColor
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Text

object ChessGUI extends JFXApp3:

  private val tileSize = 60

  // was: private var board: Board = Board.initial
  private var game: GameState = GameState.initial
  private var selected: Option[Position] = None

    private def board: Board = game.board

  private def glyph(piece: Piece): String =
    (piece.color, piece.pieceType) match
      case (Color.White, PieceType.King)   => "\u2654"
      case (Color.White, PieceType.Queen)  => "\u2655"
      case (Color.White, PieceType.Rook)   => "\u2656"
      case (Color.White, PieceType.Bishop) => "\u2657"
      case (Color.White, PieceType.Knight) => "\u2658"
      case (Color.White, PieceType.Pawn)   => "\u2659"
      case (Color.Black, PieceType.King)   => "\u265A"
      case (Color.Black, PieceType.Queen)  => "\u265B"
      case (Color.Black, PieceType.Rook)   => "\u265C"
      case (Color.Black, PieceType.Bishop) => "\u265D"
      case (Color.Black, PieceType.Knight) => "\u265E"
      case (Color.Black, PieceType.Pawn)   => "\u265F"

  private def tile(row: Int, col: Int, square: Option[Piece]): StackPane =
    val pos = Position(row, col)
    val isSelected = selected.contains(pos)
    val isLegalTarget = highlighted.contains(pos)
    val background =
      if isSelected then FxColor.rgb(130, 151, 105)        // selected piece
      else if isLegalTarget then FxColor.rgb(170, 190, 140) // legal destination
      else if (row + col) % 2 == 0 then FxColor.rgb(240, 217, 181)
      else FxColor.rgb(181, 136, 99)
    val rect = new Rectangle:
      width = tileSize
      height = tileSize
      fill = background
    val pane = new StackPane
    pane.children.add(rect)
    square match
      case Some(piece) =>
        val text = new Text(glyph(piece)):
          style = "-fx-font-size: 40px;"
        pane.children.add(text)
      case None => ()
    // Clicking this square drives the game.
    pane.onMouseClicked = _ => handleClick(pos)
    pane

      // All squares the selected piece could legally move to.
  private def legalDestinations(from: Position): Set[Position] =
    val dests =
      for
        row <- 0 until 8
        col <- 0 until 8
        target = Position(row, col)
        if CheckDetector.isLegalConsideringCheck(board, from, target)
      yield target
    dests.toSet

  // Compute once per redraw, based on current selection.
  private def highlighted: Set[Position] =
    selected match
      case Some(from) => legalDestinations(from)
      case None       => Set.empty

  // Two-click move: first click selects, second click moves.
  private def handleClick(pos: Position): Unit =
    selected match
      case None =>
        board.pieceAt(pos) match
          case Some(piece) if piece.color == game.turn => selected = Some(pos)
          case _                                       => ()
      case Some(from) =>
        if CheckDetector.isLegalConsideringCheck(board, from, pos) then
          game = game.moveMade(from, pos)
        selected = None
    redraw()


  private def redraw(): Unit =
    val status =
      if CheckDetector.isCheckmate(board, game.turn) then
        s"Checkmate — ${game.turn.opposite} wins!"
      else if CheckDetector.isStalemate(board, game.turn) then
        "Stalemate — draw"
      else if CheckDetector.isInCheck(board, game.turn) then
        s"${game.turn} to move — CHECK!"
      else
        s"${game.turn} to move"
    stage.title = s"Chess — $status"
    stage.scene().root = boardGrid(board)

  private def boardGrid(board: Board): GridPane =
    val grid = new GridPane
    for
      row <- 0 until 8
      col <- 0 until 8
    do grid.add(tile(row, col, board.pieceAt(Position(row, col))), col, row)
    grid

  override def start(): Unit =
    stage = new JFXApp3.PrimaryStage:
      title = s"Chess — ${game.turn} to move"
      scene = new Scene(tileSize * 8, tileSize * 8):
        root = boardGrid(board)
