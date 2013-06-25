package model

import scala.util.Random
import scala.annotation.tailrec

object MiniMaxSolver extends Solver {

  private val rnd = new Random

  private val SEARCH_DEPTH = 7

  def nextMove(board: GameBoard): Option[ColouredMove] = {
    if (board.isGameFull || board.isGameWon)
      None
    else
      Some(minimax(board))
  }

  private def minimax(board: GameBoard): ColouredMove = {
    val moves = nextMoves(board)
    val scores = moves map { move => minMoveValue(board.play(move), 1) }
//    println(s"top level ${scores}")
    val max = (moves zip scores) maxBy { case (mv, score) => score }
    max._1
  }

  private def maxMoveValue(board: GameBoard, depth: Int): Int = {
    if (board.isGameWon) { /*println("---- lost\n" + board.toString);*/ -1 }
    else if (board.isGameFull) 0
    else if (depth == SEARCH_DEPTH) 0
    else {
      val moves = nextMoves(board)
      val scores = moves.map(mv => minMoveValue(board.play(mv), depth + 1))
//      if (depth == 2)
//      println(s"max scores at ${depth} = ${scores}")
      scores.max
    }
  }

  private def minMoveValue(board: GameBoard, depth: Int): Int = {
    if (board.isGameWon) { /*println("++++ won\n" + board.toString); */1 }
    else if (board.isGameFull) 0
    else if (depth == SEARCH_DEPTH) 0
    else {
      val moves = nextMoves(board)
      val scores = moves.map(mv => maxMoveValue(board.play(mv), depth + 1))
//      if (depth == 1)
//      println(s"min scores at ${depth} = ${scores}")
      scores.min
    }
  }

}