package ui

import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.paint.{Color => FxColor}
import scalafx.scene.layout.{GridPane, StackPane}
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Text
import model.*

object ChessGUI extends JFXApp3:

  private val tileSize = 60

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
    val background =
      if (row + col) % 2 == 0 then FxColor.rgb(240, 217, 181)
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
    pane

  private def boardGrid(board: Board): GridPane =
    val grid = new GridPane
    for
      row <- 0 until 8
      col <- 0 until 8
    do
      grid.add(tile(row, col, board.pieceAt(Position(row, col))), col, row)
    grid

  override def start(): Unit =
    stage = new JFXApp3.PrimaryStage:
      title = "Chess"
      scene = new Scene(tileSize * 8, tileSize * 8):
        root = boardGrid(Board.initial)