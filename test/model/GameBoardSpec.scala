package model

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers

class GameBoardSpec extends WordSpec
    with MustMatchers {

  import ColouredMove._
  
  "toMap" should {

    "return a valid map given a list of single column moves" in {
      val moves = List(yellow(2), yellow(2), red(2))

      GameBoard(moves).asMap must be ===
        Map(
          (2, 1) -> RED,
          (2, 2) -> YELLOW,
          (2, 3) -> YELLOW
        )
    }

    "return a valid map given a list of moves" in {
      val moves = List(red(5), yellow(6), red(5), yellow(3), red(2), yellow(1))

      GameBoard(moves).asMap must be ===
        Map(
          (5, 1) -> RED,
          (6, 1) -> YELLOW,
          (5, 2) -> RED,
          (3, 1) -> YELLOW,
          (2, 1) -> RED,
          (1, 1) -> YELLOW
        )
    }

  }

  "toString" should {

    "return an empty board given no initial moves" in {
      GameBoard().toLines must be ===
        Seq(".......", ".......", ".......", ".......", ".......", ".......")
    }

    "return a valid board representation given some moves" in {
      val moves = List(red(5), yellow(6), red(5), yellow(3), red(2), yellow(1))

      GameBoard(moves).toLines must be ===
        Seq(".......", ".......", ".......", ".......", "....R..", "YRY.RY.")
    }

  }

  "isGameFull" should {

    "return true when it is (full)" in {
      val moves =
        (1 to 7) flatMap (n => Nil.padTo(6, red(n)))

      GameBoard(moves.toList).isGameFull must be === true
    }

    "return false when it is not (full)" in {
      val moves = List(red(5), yellow(6), red(5), yellow(3), red(2), yellow(1))

      GameBoard(moves.toList).isGameFull must be === false
    }

  }

  "isGameWon" should {

    "return true when it is won by 4 pieces of the same colour next to each other in a row" in {
      val moves = List(red(5), red(4), red(3), red(2))

      GameBoard(moves.toList).isGameWon must be === true
    }

    "return false when the board has 4 pieces of the same colour in a row interspersed with another colour" in {
      val moves = List(red(7), yellow(6), red(5), yellow(4), red(3), yellow(2), red(1))

      GameBoard(moves.toList).isGameWon must be === false
    }

    "return true when it is won by 4 pieces of the same colour next to each other in a column" in {
      val moves = List(red(5), red(5), red(5), red(5), yellow(5))

      GameBoard(moves.toList).isGameWon must be === true
    }

    "return false when the board has 4 pieces of the same colour in a column interspersed with another colour" in {
      val moves = List(red(7), yellow(7), red(7), yellow(7), red(7), red(7))

      GameBoard(moves.toList).isGameWon must be === false
    }
    
    "return true when it is won by 4 pieces of the same colour next to each other in a / diagonal" in {
      val moves = List(red(3), red(4), yellow(4), red(5), yellow(5), yellow(5), red(6), yellow(6), yellow(6), yellow(6))
      
      GameBoard(moves.toList).isGameWon must be === true
    }
    
    "return false when the board has 4 pieces of the same colour in a / diagonal interspersed with another colour" in {
      val moves = List(red(3), yellow(4), red(4), red(5), yellow(5), yellow(5), red(6), yellow(6), red(6), yellow(6), red(7), yellow(7), red(7), yellow(7), red(7))

      GameBoard(moves.toList).isGameWon must be === false
    }
    
    "return true when it is won by 4 pieces of the same colour next to each other in a \\ diagonal" in {
      val moves = List(red(5), red(4), yellow(4), red(3), yellow(3), yellow(3), red(2), yellow(2), yellow(2), yellow(2))
      
      GameBoard(moves.toList).isGameWon must be === true
    }
    
    "return false when the board has 4 pieces of the same colour in a \\ diagonal interspersed with another colour" in {
      val moves = List(red(5), yellow(4), red(4), red(3), yellow(3), yellow(3), red(2), yellow(2), red(2), yellow(2), red(1), yellow(1), red(1), yellow(1), red(1))

      GameBoard(moves.toList).isGameWon must be === false
    }
    
  }
  
  "gridToGameBoard" should {
    
    "return an empty GameBoard from a sequence of moves represented as ..." in {
      GameBoard.gridToGameBoard(List.fill(3)("...")) must be === GameBoard()
    }
    
    "return a GameBoard from a sequence of moves represented as ....R.." in {
      GameBoard.gridToGameBoard(List("..Y.", "..R.", ".RY.")) must be === GameBoard(List(yellow(3), red(3), red(2), yellow(3)))
    }
  
  }
  
}