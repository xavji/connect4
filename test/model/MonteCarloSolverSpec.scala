package model

import org.junit.runner.RunWith
import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MonteCarloSolverSpec extends WordSpec
    with MustMatchers {

  
  import MonteCarloSolver.nextMove
  import ColouredMove._
  
  "A MonteCarloSolver" should {

    "return a RED move in column 3 if the board is empty" in {
      nextMove(GameBoard()) must be === Some(red(3))
    }
    
    "return a YELLOW move in column 4 if the board has RED(1)" in {
      nextMove(GameBoard(List(red(1)))) must be === Some(yellow(4))
    }
    
  }

}