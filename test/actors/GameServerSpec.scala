package actors

import akka.actor.ActorSystem
import akka.actor.Props
import akka.testkit.ImplicitSender
import akka.testkit.TestKit

import org.junit.runner.RunWith

import org.scalatest.WordSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.MustMatchers

import model._

@RunWith(classOf[JUnitRunner])
class GameServerSpec extends TestKit(ActorSystem("GameServerSpecSystem"))
    with WordSpec
    with MustMatchers
    with StopSystemAfterAll
    with ImplicitSender {

  val gameServer = system.actorOf(Props[GameServer], "game-server-spec")

  "consecutive CreateGame messages obtain successive Game reply ids" in {
    gameServer ! CreateGame

    expectMsg(Game(1))

    gameServer ! CreateGame

    expectMsg(Game(2))
  }

  "RegistrationRequest obtains successively RED and YELLOW Registration replies" in {
    gameServer ! CreateGame
    expectMsg(Game(3))

    val regRequest = RegistrationRequest(3)

    gameServer ! regRequest
    val redReg = expectMsgClass(classOf[Registration])
    redReg.colour must be === RED

    gameServer ! regRequest
    val yellowReg = expectMsgClass(classOf[Registration])
    yellowReg.colour must be === YELLOW
  }

  "RegistrationRequest gets an InvalidRequest if the game has 2 players already" in {
    gameServer ! RegistrationRequest(3)
    expectMsg(InvalidRequest(RegistrationRequest(3)))
  }

  "GameMove obtains an InvalidRequest reply if a player sends a move when it is not his turn" in {
    val (gameId, _, yellowId) = setUpNewGame()
    val move = GameMove(gameId, yellowId, 3)

    gameServer ! move
    expectMsg(InvalidRequest(move))
  }

  "GameMove obtains a reply with MoveStatus if a player sends a valid move" in {
    val (gameId, redId, _) = setUpNewGame()

    gameServer ! GameMove(gameId, redId, 2)
    expectMsg(MoveStatus(List(".......", ".......", ".......", ".......", ".......", ".R....."), false))
  }

  "GameMove obtains an InvalidRequest reply if a player sends a move in a column that does not exist on his turn" in {
    val (gameId, redId, _) = setUpNewGame()
    val move = GameMove(gameId, redId, 53)

    gameServer ! move
    expectMsg(InvalidRequest(move))
  }

  "GameMove obtains an InvalidRequest reply if a player sends a move in a column that is full on his turn" in {
    val (gameId, redId, yellowId) = setUpNewGame()

    (1 to 6) foreach { n =>
      val playerId =
        if (n % 2 == 1)
          redId
        else
          yellowId
      gameServer ! GameMove(gameId, playerId, 5)
      expectMsgClass(classOf[MoveStatus])
    }

    val invalid = GameMove(gameId, redId, 5)
    gameServer ! invalid
    expectMsg(InvalidRequest(invalid))
  }

  "Two GameMove from the same player obtains replies with MoveStatus then InvalidRequest" in {
    val (gameId, redId, _) = setUpNewGame()

    gameServer ! GameMove(gameId, redId, 2)
    expectMsg(MoveStatus(List(".......", ".......", ".......", ".......", ".......", ".R....."), false))

    gameServer ! GameMove(gameId, redId, 2)
    expectMsg(InvalidRequest(GameMove(gameId, redId, 2)))
  }

  "PlayerStatusRequest, GameMove, then PlayerStatusRequest with player who should be playing next" in {
    val (gameId, redId, yellowId) = setUpNewGame()

    gameServer ! PlayerStatusRequest(gameId, redId)
    expectMsg(PlayerStatus(List(".......", ".......", ".......", ".......", ".......", "......."), true))

    gameServer ! GameMove(gameId, redId, 2)
    expectMsg(MoveStatus(List(".......", ".......", ".......", ".......", ".......", ".R....."), false))

    gameServer ! PlayerStatusRequest(gameId, redId)
    expectMsg(PlayerStatus(List(".......", ".......", ".......", ".......", ".......", ".R....."), false))

    gameServer ! PlayerStatusRequest(gameId, yellowId)
    expectMsg(PlayerStatus(List(".......", ".......", ".......", ".......", ".......", ".R....."), true))
  }

  "PlayerStatusRequest when a game has not started yet, still waiting for the second player to join" in {
    val (game, regRequest, redReg) = setUpRedGame

    gameServer ! PlayerStatusRequest(game.id, redReg.playerId)
    expectMsg(PlayerStatus(List(".......", ".......", ".......", ".......", ".......", "......."), false))

    gameServer ! regRequest
    val yellowReg = expectMsgClass(classOf[Registration])

    gameServer ! PlayerStatusRequest(game.id, redReg.playerId)
    expectMsg(PlayerStatus(List(".......", ".......", ".......", ".......", ".......", "......."), true))
  }

  "PlayerStatusRequest when a game has not started yet, waiting for the first player to join" in {
    gameServer ! CreateGame
    val game = expectMsgClass(classOf[Game])

    val invalidReq = PlayerStatusRequest(game.id, "unknown id") 
    gameServer ! invalidReq
    
    expectMsg(InvalidRequest(invalidReq))
  }
  
  "PlayerStatusRequest shows the winner when the game is won and no moves are accepted" in {
    val (gameId, redId, yellowId) = setUpNewGame()

    (1 to 3) foreach { n =>
      gameServer ! GameMove(gameId, redId, 2)
      gameServer ! GameMove(gameId, yellowId, 3)
    }
    receiveWhile() {
      case mv: MoveStatus =>
    }

    val finalBoard = List(".......", ".......", ".R.....", ".RY....", ".RY....", ".RY....")
    gameServer ! GameMove(gameId, redId, 2)
    expectMsg(MoveStatus(finalBoard, true))

    gameServer ! PlayerStatusRequest(gameId, yellowId)
    expectMsg(PlayerStatus(finalBoard, false, Some(RED)))

    gameServer ! PlayerStatusRequest(gameId, redId)
    expectMsg(PlayerStatus(finalBoard, false, Some(RED)))

    gameServer ! GameMove(gameId, yellowId, 3)
    expectMsg(InvalidRequest(GameMove(gameId, yellowId, 3)))
  }

  private def setUpNewGame() = {
    val (game, regRequest, redReg) = setUpRedGame

    gameServer ! regRequest
    val yellowReg = expectMsgClass(classOf[Registration])
    (game.id, redReg.playerId, yellowReg.playerId)
  }

  private def setUpRedGame: (model.Game, model.RegistrationRequest, model.Registration) = {
    gameServer ! CreateGame
    val game = expectMsgClass(classOf[Game])

    val regRequest = RegistrationRequest(game.id)

    gameServer ! regRequest
    val redReg = expectMsgClass(classOf[Registration])
    
    (game, regRequest, redReg)
  }

}
