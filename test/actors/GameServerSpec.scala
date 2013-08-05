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
import akka.actor.ActorRef

@RunWith(classOf[JUnitRunner])
class GameServerSpec extends TestKit(ActorSystem("GameServerSpecSystem"))
    with WordSpec
    with MustMatchers
    with StopSystemAfterAll
    with ImplicitSender {

  def gameServer = system.actorOf(Props[GameServer])

  "consecutive CreateGame messages obtain successive Game reply ids" in {
    val gs = gameServer
    
    gs ! CreateGame
    expectMsg(Game(0))

    gs ! CreateGame
    expectMsg(Game(1))
  }

  "RegistrationRequest obtains successively RED and YELLOW Registration replies" in {
    val gs = gameServer
    
    gs ! CreateGame
    expectMsg(Game(0))

    val regRequest = RegistrationRequest(0)

    gs ! regRequest
    val redReg = expectMsgClass(classOf[Registration])
    redReg.colour must be === RED

    gs ! regRequest
    val yellowReg = expectMsgClass(classOf[Registration])
    yellowReg.colour must be === YELLOW
  }

  "RegistrationRequest gets an InvalidRequest if the game has 2 players already" in {
    val gs = gameServer
    val req = RegistrationRequest(0)
    
    gs ! CreateGame
    expectMsgClass(classOf[Game])
    gs ! req
    expectMsgClass(classOf[Registration])
    gs ! req
    expectMsgClass(classOf[Registration])
    
    gs ! req
    expectMsg(InvalidRequest(req))
  }

  "GameMove obtains an InvalidRequest reply if a player sends a move when it is not his turn" in {
    val gs = gameServer
    val (gameId, _, yellowId) = setUpNewGame(gs)
    val move = GameMove(gameId, yellowId, 3)

    gs ! move
    expectMsg(InvalidRequest(move))
  }

  "GameMove obtains a reply with MoveStatus if a player sends a valid move" in {
    val gs = gameServer
    val (gameId, redId, _) = setUpNewGame(gs)

    gs ! GameMove(gameId, redId, 2)
    expectMsg(MoveStatus(List(".......", ".......", ".......", ".......", ".......", ".R....."), false))
  }

  "GameMove obtains an InvalidRequest reply if a player sends a move in a column that does not exist on his turn" in {
    val gs = gameServer
    val (gameId, redId, _) = setUpNewGame(gs)
    val move = GameMove(gameId, redId, 53)

    gs ! move
    expectMsg(InvalidRequest(move))
  }

  "GameMove obtains an InvalidRequest reply if a player sends a move in a column that is full on his turn" in {
    val gs = gameServer
    val (gameId, redId, yellowId) = setUpNewGame(gs)

    (1 to 6) foreach { n =>
      val playerId =
        if (n % 2 == 1)
          redId
        else
          yellowId
      gs ! GameMove(gameId, playerId, 5)
      expectMsgClass(classOf[MoveStatus])
    }

    val invalid = GameMove(gameId, redId, 5)
    gs ! invalid
    expectMsg(InvalidRequest(invalid))
  }

  "Two GameMove from the same player obtains replies with MoveStatus then InvalidRequest" in {
    val gs = gameServer
    val (gameId, redId, _) = setUpNewGame(gs)

    gs ! GameMove(gameId, redId, 2)
    expectMsg(MoveStatus(List(".......", ".......", ".......", ".......", ".......", ".R....."), false))

    gs ! GameMove(gameId, redId, 2)
    expectMsg(InvalidRequest(GameMove(gameId, redId, 2)))
  }

  "PlayerStatusRequest, GameMove, then PlayerStatusRequest with player who should be playing next" in {
    val gs = gameServer
    val (gameId, redId, yellowId) = setUpNewGame(gs)

    gs ! PlayerStatusRequest(gameId, redId)
    expectMsg(PlayerStatus(List(".......", ".......", ".......", ".......", ".......", "......."), true))

    gs ! GameMove(gameId, redId, 2)
    expectMsg(MoveStatus(List(".......", ".......", ".......", ".......", ".......", ".R....."), false))

    gs ! PlayerStatusRequest(gameId, redId)
    expectMsg(PlayerStatus(List(".......", ".......", ".......", ".......", ".......", ".R....."), false))

    gs ! PlayerStatusRequest(gameId, yellowId)
    expectMsg(PlayerStatus(List(".......", ".......", ".......", ".......", ".......", ".R....."), true))
  }

  "PlayerStatusRequest when a game has not started yet, still waiting for the second player to join" in {
    val gs = gameServer
    val (game, regRequest, redReg) = setUpRedGame(gs)

    gs ! PlayerStatusRequest(game.id, redReg.playerId)
    expectMsg(PlayerStatus(List(".......", ".......", ".......", ".......", ".......", "......."), false))

    gs ! regRequest
    val yellowReg = expectMsgClass(classOf[Registration])

    gs ! PlayerStatusRequest(game.id, redReg.playerId)
    expectMsg(PlayerStatus(List(".......", ".......", ".......", ".......", ".......", "......."), true))
  }

  "PlayerStatusRequest when a game has not started yet, waiting for the first player to join" in {
    val gs = gameServer
    gs ! CreateGame
    val game = expectMsgClass(classOf[Game])

    val invalidReq = PlayerStatusRequest(game.id, "unknown id") 
    gs ! invalidReq
    
    expectMsg(InvalidRequest(invalidReq))
  }
  
  "PlayerStatusRequest shows the winner when the game is won and no moves are accepted" in {
    val gs = gameServer
    val (gameId, redId, yellowId) = setUpNewGame(gs)

    (1 to 3) foreach { n =>
      gs ! GameMove(gameId, redId, 2)
      expectMsgClass(classOf[MoveStatus])
      gs ! GameMove(gameId, yellowId, 3)
      expectMsgClass(classOf[MoveStatus])
    }
    val finalBoard = List(".......", ".......", ".R.....", ".RY....", ".RY....", ".RY....")
    gs ! GameMove(gameId, redId, 2)
    expectMsg(MoveStatus(finalBoard, true))

    gs ! PlayerStatusRequest(gameId, yellowId)
    expectMsg(PlayerStatus(finalBoard, false, Some(RED)))

    gs ! PlayerStatusRequest(gameId, redId)
    expectMsg(PlayerStatus(finalBoard, false, Some(RED)))

    gs ! GameMove(gameId, yellowId, 3)
    expectMsg(InvalidRequest(GameMove(gameId, yellowId, 3)))
  }

  "ListGames responds with a list of game statuses" in {
    val gs = gameServer
    setUpNewGame(gs)

    gs ! CreateGame
    expectMsg(Game(1))

    gs ! CreateGame
    expectMsg(Game(2))
    gs ! RegistrationRequest(2)
    expectMsgClass(classOf[Registration])

    gs ! ListGames
    val games = expectMsgClass(classOf[ListGamesResponse])

    games.statuses must be === List(
      GameStatus(0, AwaitingMove(RED)),
      GameStatus(1, Registering(RED)),
      GameStatus(2, Registering(YELLOW))
    )
  }
  
  private def setUpNewGame(gs: ActorRef) = {
    val (game, regRequest, redReg) = setUpRedGame(gs)

    gs ! regRequest
    val yellowReg = expectMsgClass(classOf[Registration])
    (game.id, redReg.playerId, yellowReg.playerId)
  }

  private def setUpRedGame(gs: ActorRef): (model.Game, model.RegistrationRequest, model.Registration) = {
    gs ! CreateGame
    val game = expectMsgClass(classOf[Game])

    val regRequest = RegistrationRequest(game.id)

    gs ! regRequest
    val redReg = expectMsgClass(classOf[Registration])
    
    (game, regRequest, redReg)
  }

}
