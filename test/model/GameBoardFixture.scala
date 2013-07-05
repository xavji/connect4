package model

import ColouredMove._
import Dims._

trait GameBoardFixture {

  val fullBoard: GameBoard = {
    val redFn = red _
    val yellowFn = yellow _
    val moves = (1 to LINES) flatMap {
      line =>
        val (fn1, fn2) = line match {
	      case n if (n % 2 == 0) => (redFn, yellowFn)
	      case _ => (yellowFn, redFn)
        }
        (1 to COLS) map {
          col =>
            if (col <= 3) fn1(col)
            else if (col <= 6) fn2(col)
            else fn1(col)
        }
    }
    GameBoard(moves.toList)
  }
  
  
}