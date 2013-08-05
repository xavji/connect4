package robot

import akka.actor._
import model._
import java.util.concurrent.CountDownLatch

class PollingPlayerActor(restGateway: RestGateway, solver: Solver, latch: CountDownLatch) extends Actor with LoggingFSM[PlayerState, GameBoard] {

  type PlayerEventHander = PartialFunction[Event, FSM.State[PlayerState, GameBoard]]
  
  startWith(WaitingForTurn, GameBoard())

  when(WaitingForTurn) {
    case Event(PollForTurn, _) =>
      restGateway.pollForTurn(self)
      goto(WaitingForTurnResponse)
  }

  when(WaitingForTurnResponse) {
    case Event(TurnFeedBackFailed, _) => 
      goto(WaitingForTurn)
    case Event(TurnFeedBack(false, board), _) =>
      val newState =
        if (board.isGameWon) Lost
        else WaitingForTurn
      goto(newState) using (board)
    case Event(TurnFeedBack(true, board), _) =>
      if (board.isGameFull) goto(Drawn) using(board)
      else
	      solver.nextMove(board)
		      .map {
		        move =>
		          restGateway.play(move.column, self)
		          goto(Playing) using (board)
		      }
		      .getOrElse(stay)
    case Event(PollForTurn, _) => 
      stay
  }

  when(Playing) {
    case Event(MoveFeedBackFailed, _)         => goto(WaitingForTurn)
    case Event(MoveFeedBack(true, board), _)  => goto(Won) using (board)
    case Event(MoveFeedBack(false, board), _) => if (!board.isGameFull) goto(WaitingForTurn) using (board) else goto(Drawn) using (board)
    case Event(PollForTurn, _)                => stay
  }

  when(Won)(gameOverHandler)
  when(Lost)(gameOverHandler)
  when(Drawn)(gameOverHandler)

  private def gameOverHandler: PlayerEventHander = {
    case _ =>
      val result = stateName.toString().toLowerCase()
      println(s"Game was $result\n$stateData\n")
      latch.countDown()
      stop
  }

  initialize

}
