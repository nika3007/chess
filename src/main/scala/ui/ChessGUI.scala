package ui

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

  // MUTABLE state: the current board, and the first-clicked square (if any).
  // This is deliberate — a GUI is inherently stateful (what's on screen changes).
  private var board: Board = Board.initial
  private var selected: Option[Position] = None

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
    val background =
      if isSelected then FxColor.rgb(130, 151, 105) // highlight selected
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

  // Two-click move: first click selects, second click moves.
  private def handleClick(pos: Position): Unit =
    selected match
      case None =>
        // First click: select this square only if it holds a piece.
        if board.pieceAt(pos).isDefined then selected = Some(pos)
      case Some(from) =>
        // Second click: move from the selected square to here, then clear selection.
        board = board.move(from, pos)
        selected = None
    redraw()

  private def redraw(): Unit =
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
      title = "Chess"
      scene = new Scene(tileSize * 8, tileSize * 8):
        root = boardGrid(board)
