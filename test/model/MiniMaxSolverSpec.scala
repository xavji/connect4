package model

import org.junit.runner.RunWith
import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MiniMaxSolverSpec extends WordSpec
    with MustMatchers {

  
  import ColouredMove._
  import MiniMaxSolver.nextMove
  
  "A MiniMaxSolver" should {

    "return a YELLOW move in column 3 or 4 if the board has RED(1)" ignore {
      val next = nextMove(GameBoard(List(red(1)))) 
      next must (equal(Some(yellow(3))) or equal(Some(yellow(4))))
    }
    
  }

  
}