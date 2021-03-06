package robot

import java.util.concurrent.CountDownLatch

import akka.actor.ActorSystem
import akka.testkit.TestFSMRef
import akka.testkit.TestKit

import org.junit.runner.RunWith
import org.scalamock.scalatest.MockFactory
import org.scalatest.WordSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.MustMatchers

import model._

@RunWith(classOf[JUnitRunner])
class PollingPlayerActorSpec extends TestKit(ActorSystem("PollingPlayerActorSpec"))
    with WordSpec
    with MustMatchers
    with MockFactory
    with GameBoardFixture {

  import ColouredMove._

  val gameWonBoard = GameBoard(List.fill(4)(red(1)))

  "Initial state name is WaitingForTurn and data is an empty GameBoard" in {
    val (player, _, _) = fixture
    player.stateName must be === WaitingForTurn
    player.stateData must be === GameBoard()
  }

  "WaitingForTurn x PollForTurn triggers a pollForTurn on the RestGateway " +
  "and transitions to WaitingForTurnResponse" in {
    val (player, gateway, solver) = fixture

    (gateway.pollForTurn _).expects(player)

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

  "WaitingForTurnResponse x TurnFeedBack when NOT player's turn and game is won " +
    "transitions to Lost" in {
      val (player, gateway, solver) = fixture

      player.setState(WaitingForTurnResponse)

      player ! TurnFeedBack(false, gameWonBoard)

      player.stateName must be === Lost
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

  "WaitingForTurnResponse x TurnFeedBack when player's turn " +
    "but board is full transitions to Drawn" in {
      val (player, gateway, solver) = fixture

      player.setState(WaitingForTurnResponse)

      player ! TurnFeedBack(true, fullBoard)

      player.stateName must be === Drawn
      player.stateData must be === fullBoard
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

  "Playing x MoveFeedBack when the game is won transitions to Won" in {
    val (player, gateway, solver) = fixture

    player.setState(Playing)

    player ! MoveFeedBack(true, gameWonBoard)

    player.stateName must be === Won
    player.stateData must be === gameWonBoard
  }

  "Playing x MoveFeedBack when the game is drawn transitions to Drawn" in {
    val (player, gateway, solver) = fixture

    player.setState(Playing)

    player ! MoveFeedBack(false, fullBoard)

    player.stateName must be === Drawn
    player.stateData must be === fullBoard
  }

  "Playing x MoveFeedBack when the game is not won transitions to WaitingForTurn" in {
    val (player, gateway, solver) = fixture
    val board = GameBoard(List(yellow(6)))

    player.setState(Playing)

    player ! MoveFeedBack(false, board)

    player.stateName must be === WaitingForTurn
    player.stateData must be === board
  }

  private def fixture: (TestFSMRef[PlayerState, GameBoard, PollingPlayerActor], RestGateway, Solver) = {
    val gateway = mock[RestGateway]
    val solver = mock[Solver]
    (TestFSMRef(new PollingPlayerActor(gateway, solver, new CountDownLatch(1))), gateway, solver)
  }

}
