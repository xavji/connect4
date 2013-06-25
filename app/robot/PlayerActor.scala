package robot

import akka.actor.LoggingFSM
import akka.actor.Actor
import model._

class PlayerActor(restGateway: RestGateway, solver: Solver) extends Actor with LoggingFSM[PlayerState, GameBoard] {

  startWith(WaitingForTurn, GameBoard())

  when(WaitingForTurn) {
    case Event(PollForTurn, _) =>
      restGateway.pollForTurn(self)
      goto(WaitingForTurnResponse)
  }

  when(WaitingForTurnResponse) {
    case Event(TurnFeedBackFailed, _) => goto(WaitingForTurn)
    case Event(TurnFeedBack(false, board), _) =>
      val newState =
        if (board.isGameWon) Done
        else WaitingForTurn
      goto(newState) using (board)
    case Event(TurnFeedBack(true, board), _) =>
      solver.nextMove(board)
	      .map {
	        move =>
	          restGateway.play(move.column, self)
	          goto(Playing) using (board)
	      }
	      .getOrElse(stay)
    case Event(PollForTurn, _) => stay
  }

  when(Playing) {
    case Event(MoveFeedBackFailed, _)         => goto(WaitingForTurn)
    case Event(MoveFeedBack(true, board), _)  => goto(Done) using (board)
    case Event(MoveFeedBack(false, board), _) => goto(WaitingForTurn) using (board)
    case Event(PollForTurn, _)                => stay
  }

  when(Done) {
    case _ => stay
  }

  onTransition {
    case _ -> Done =>
      val status = if (nextStateData.isGameWon) "won" else "lost"
      println(s"Game was $status\n$nextStateData\n")
  }

  initialize

}
