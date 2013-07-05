package model

import scala.util.Random
import scala.annotation.tailrec

trait Solver {

  def nextMoves(board: GameBoard): Seq[ColouredMove] =
    (1 to Dims.COLS) map { col => move(col, board) } filter (board.canPlay(_))

  def move(col: Int, board: GameBoard): ColouredMove = 
    ColouredMove(col, board.moves.head.colour.other)

  def homeColour(board: GameBoard): Colour =
    board.moves.head.colour.other

  def nextMove(board: GameBoard): Option[ColouredMove]

}

class MonteCarloSolver(playouts: Int = 5000, searchDepth: Int = 10) extends Solver {

  private val rnd = new Random

  def nextMove(board: GameBoard): Option[ColouredMove] = {
    board.moves match {
      case Nil => Some(ColouredMove(3, RED))
      case _ =>
        val scoringFn = scoreSimulation(homeColour(board))
        val moves = nextMoves(board)
        val sims = moves.par map { mv =>
          monteCarloSimulation(board.play(mv))
        }
        val scored = sims.map(scoringFn)
        scored.indexOf(scored.max) match {
          case i if (i >= 0) => Some(moves(i))
          case _             => None
        }
    }
  }
  
  private def scoreSimulation(homeColour: Colour): Seq[GameBoard] => Int =
    boards =>
      boards.foldLeft(0) { (acc, brd) =>
        if (brd.isGameWon) {
          if (homeColour == brd.moves.head.colour) acc + 1
          else acc - 1
        } else acc + 0
      }

  private def monteCarloSimulation(board: GameBoard): Seq[GameBoard] = {
    (1 to playouts) map { n =>
      (1 to searchDepth).toSeq.foldLeft(board) { (brd, k) =>
        randomMove(brd, 0) map { move =>
          brd.play(move)
        } getOrElse brd
      }
    }
  }

  @tailrec
  private def randomMove(board: GameBoard, depth: Int): Option[ColouredMove] = {
    val col = rnd.nextInt(Dims.COLS) + 1
    val mv = move(col, board)
    if (board.canPlay(mv))
      Some(mv)
    else if (depth == 8)
      None
    else
      randomMove(board, depth + 1)
  }

}