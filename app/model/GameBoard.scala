package model

object Dims {
  val COLS = 7
  val LINES = 6
  val WINNING = 4
}

case class GameBoard(moves: List[ColouredMove] = Nil) {

  import Dims._

  private def isValidColumn(move: ColouredMove): Boolean = move.column >= 1 && move.column <= COLS

  private def isColumnAvailable(move: ColouredMove): Boolean = moves.filter(_.column == move.column).size < LINES

  def canPlay(move: ColouredMove): Boolean = isValidColumn(move) && isColumnAvailable(move) && !isGameFull && !isGameWon

  def isGameWon: Boolean = hasWinnerInARow || hasWinnerInAColumn || hasWinnerInUpDiagonal || hasWinnerInDownDiagonal

  def isGameFull: Boolean = moves.size >= COLS * LINES

  lazy val asMap: Map[(Int, Int), Colour] =
    moves.reverse.foldLeft(Map[(Int, Int), Colour]()) {
      (map, move) =>
        val sameColumn = map filterKeys { case (i, j) => i == move.column }
        map + ((move.column, sameColumn.size + 1) -> move.colour)
    }

  def toLines: Seq[String] =
    (LINES to 1 by -1) map { line =>
      (1 to COLS) map (col => stringMap((col, line))) mkString
    }

  def play(move: ColouredMove) = copy(moves = move :: moves)

  override def toString = toLines mkString "\n"

  private def stringMap =
    asMap map {
      case (pos, colour) if (colour == RED) => pos -> "R"
      case (pos, _)                         => pos -> "Y"
    } withDefaultValue "."

  private def hasWinnerInARow: Boolean =
    hasWinner(LINES, COLS + 1 - WINNING, (startCoord, index) => (startCoord._1 + index, startCoord._2))

  private def hasWinnerInAColumn: Boolean =
    hasWinner(LINES + 1 - WINNING, COLS, (startCoord, index) => (startCoord._1, startCoord._2 + index))

  private def hasWinnerInUpDiagonal: Boolean =
    hasWinner(LINES + 1 - WINNING, COLS + 1 - WINNING, (startCoord, index) => (startCoord._1 + index, startCoord._2 + index))

  private def hasWinnerInDownDiagonal: Boolean =
    hasWinner(LINES, COLS + 1 - WINNING, (startCoord, index) => (startCoord._1 + index, startCoord._2 - index))

  private def hasWinner(toLine: Int, toColumn: Int, piecePositioner: ((Int, Int), Int) => (Int, Int)): Boolean = {
    (1 to toLine) exists { line =>
      (1 to toColumn) exists { colStartIndex =>
        asMap.get((colStartIndex, line))
          .map { colStartMove =>
            (1 until WINNING) forall { pieceIndex =>
              val otherPieceCoord = piecePositioner((colStartIndex, line), pieceIndex)
              asMap.get(otherPieceCoord).map(_ == colStartMove).getOrElse(false)
            }
          }
          .getOrElse(false)
      }
    }
  }
}

object GameBoard {

  private val charToColouredMove: PartialFunction[(Char, Int), ColouredMove] = {
    case ('R', idx) => ColouredMove(idx + 1, RED)
    case ('Y', idx) => ColouredMove(idx + 1, YELLOW)
  }
  
  def gridToGameBoard(grid: Seq[String]): GameBoard = {
    val moves = grid.toList.map(row => row.toList).flatMap(chars => chars.zipWithIndex.collect(charToColouredMove))
    GameBoard(moves)
  }

}

case class ColouredMove(column: Int, colour: Colour)

object ColouredMove {

  def red(n: Int) = ColouredMove(n, RED)

  def yellow(n: Int) = ColouredMove(n, YELLOW)

}
