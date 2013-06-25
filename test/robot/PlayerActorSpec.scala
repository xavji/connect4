package robot

import akka.actor.ActorSystem
import akka.testkit.TestFSMRef
import akka.testkit.TestKit

import org.scalamock.scalatest.MockFactory
import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers

import model._

class PlayerActorSpec extends TestKit(ActorSystem("testsystem"))
	with WordSpec
    with MustMatchers
    with MockFactory {
  
  import ColouredMove._
  
  val gameWonBoard = GameBoard(List.fill(4)(red(1)))
  
  "WaitingForTurn x PollForTurn triggers a pollForTurn on the RestGateway " + 
  "and transitions to WaitingForTurnResponse" in {
	val (player, gateway, solver) = fixture
	
	(gateway.pollForTurn _).expects(player)
	
	player.stateName must be === WaitingForTurn
	player.stateData must be === GameBoard()
	
    player ! PollForTurn

	player.stateName must be === WaitingForTurnResponse
  }

  "WaitingForTurnResponse x PollForTurn does nothing" in {
	val (player, gateway, solver) = fixture
	
	player.setState(WaitingForTurnResponse)
	
    player ! PollForTurn

	player.stateName must be === WaitingForTurnResponse
  }
  
  "WaitingForTurnResponse x TurnFeedBack when NOT player's turn and game is not over " +
  "transitions to WaitingForTurn" in {
	val (player, gateway, solver) = fixture
	
	player.setState(WaitingForTurnResponse)
	
    player ! TurnFeedBack(false, GameBoard())

	player.stateName must be === WaitingForTurn
  }
  
  "WaitingForTurnResponse x TurnFeedBack when NOT player's turn and game is over " +
  "transitions to Done" in {
	val (player, gateway, solver) = fixture
	
	player.setState(WaitingForTurnResponse)
	
    player ! TurnFeedBack(false, gameWonBoard)

	player.stateName must be === Done
	player.stateData must be === gameWonBoard
  }
  
  "WaitingForTurnResponse x TurnFeedBackTimeOut transitions to WaitingForTurn" in {
	val (player, gateway, solver) = fixture
	
	player.setState(WaitingForTurnResponse)
	
    player ! TurnFeedBackFailed

	player.stateName must be === WaitingForTurn
  }
  
  "WaitingForTurnResponse x TurnFeedBack when player's turn " + 
  "triggers a play on the RestGateway and transitions to Playing" in {
	val (player, gateway, solver) = fixture
	val board = GameBoard(List(yellow(2)))
	
	(solver.nextMove _).expects(board).returning(Some(red(3)))
	(gateway.play _).expects(3, player)
	
	player.setState(WaitingForTurnResponse)
	
    player ! TurnFeedBack(true, board)

	player.stateName must be === Playing
	player.stateData must be === board
  }
  
  "Playing x MoveFeedBackTimeOut transitions to WaitingForTurn" in {
	val (player, gateway, solver) = fixture
	
	player.setState(Playing)
	
    player ! MoveFeedBackFailed

	player.stateName must be === WaitingForTurn
  }
  
  "Playing x PollForTurn does nothing" in {
	val (player, gateway, solver) = fixture
	
	player.setState(Playing)
	
    player ! PollForTurn

	player.stateName must be === Playing
  }
  
  "Playing x MoveFeedBack when the game is won transitions to Done" in {
	val (player, gateway, solver) = fixture
	
	player.setState(Playing)
	
    player ! MoveFeedBack(true, gameWonBoard)

	player.stateName must be === Done
	player.stateData must be === gameWonBoard
  }
  
  "Playing x MoveFeedBack when the game is not won transitions to WaitingForTurn" in {
	val (player, gateway, solver) = fixture
	val board = GameBoard(List(yellow(6)))
	
	player.setState(Playing)
	
    player ! MoveFeedBack(false, board)

	player.stateName must be === WaitingForTurn
	player.stateData must be === board
  }
  
  private def fixture: (TestFSMRef[PlayerState, GameBoard, PlayerActor], RestGateway, Solver) = {
    val gateway = mock[RestGateway]
    val solver = mock[Solver]
    (TestFSMRef(new PlayerActor(gateway, solver)), gateway, solver)
  }
  
}
