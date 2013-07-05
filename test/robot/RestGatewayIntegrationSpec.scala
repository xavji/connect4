package robot

import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.junit.runner.RunWith
import org.scalatest.WordSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.MustMatchers
import play.api.libs.ws.WS
import play.api.test._
import play.api.test.Helpers._
import actors.StopSystemAfterAll
import model._
import model.ColouredMove._
import util.Connect4Urls
import scala.collection.mutable.ListBuffer

@RunWith(classOf[JUnitRunner])
class RestGatewayIntegrationSpec extends TestKit(ActorSystem("RestGatewaySpecSystem"))
    with WordSpec
    with MustMatchers
    with StopSystemAfterAll
    with Connect4Urls {

  val port = 3333
  val GameId = """\{ "game": \{ "id": (\d+) \} \}""".r

  "A RestGateway" should {

    "poll for turn for both players at the start of the game" in {
      running(TestServer(port), HTMLUNIT) { browser =>
        val (redRest, yellowRest) = buildRedAndYellowGateways(browser)

        redRest.pollForTurn(testActor)
        expectMsg(TurnFeedBack(true, GameBoard()))
        
        
        yellowRest.pollForTurn(testActor)
        expectMsg(TurnFeedBack(false, GameBoard()))
      }
    }

    "red plays at the start of the game" in {
      running(TestServer(port), HTMLUNIT) { browser =>
        val (redRest, yellowRest) = buildRedAndYellowGateways(browser)

        redRest.play(2, testActor)
        expectMsg(MoveFeedBack(false, GameBoard(List(red(2)))))
      }
    }
    
    "yellow plays after red" in {
      running(TestServer(port), HTMLUNIT) { browser =>
        val (redRest, yellowRest) = buildRedAndYellowGateways(browser)

        redRest.play(4, testActor)
        expectMsg(MoveFeedBack(false, GameBoard(List(red(4)))))
        
        yellowRest.play(1, testActor)
        expectMsg(MoveFeedBack(false, GameBoard(List(yellow(1), red(4)))))
      }
    }
    
    "yellow plays when it is not his turn" in {
      running(TestServer(port), HTMLUNIT) { browser =>
        val (redRest, yellowRest) = buildRedAndYellowGateways(browser)

        yellowRest.play(4, testActor)
        expectMsg(MoveFeedBackFailed)
      }
    }
    
    "poll for turn for both players after two moves" in {
      running(TestServer(port), HTMLUNIT) { browser =>
        val (redRest, yellowRest) = buildRedAndYellowGateways(browser)
        
        redRest.play(4, testActor)
        expectMsg(MoveFeedBack(false, GameBoard(List(red(4)))))
        
        val gameBoardAfterTwoMoves = GameBoard(List(yellow(1), red(4)))
        
        yellowRest.play(1, testActor)
        expectMsg(MoveFeedBack(false, gameBoardAfterTwoMoves))
        
        redRest.pollForTurn(testActor)
        expectMsg(TurnFeedBack(true, gameBoardAfterTwoMoves))
        
        yellowRest.pollForTurn(testActor)
        expectMsg(TurnFeedBack(false, gameBoardAfterTwoMoves))
      }
    }
    
    "winning move gives correct MoveFeedBack and then pollForTurn says nobody can play" in {
      running(TestServer(port), HTMLUNIT) { browser =>
        val (redRest, yellowRest) = buildRedAndYellowGateways(browser)
        
        val buffer = ListBuffer[ColouredMove]()
        (1 to 3) foreach {
          i =>
	        redRest.play(4, testActor)
	        buffer.prepend(red(4))
	        expectMsg(MoveFeedBack(false, GameBoard(buffer.toList)))
	        yellowRest.play(2, testActor)
	        buffer.prepend(yellow(2))
	        expectMsg(MoveFeedBack(false, GameBoard(buffer.toList)))
        }
        
        redRest.play(4, testActor)
	    buffer.prepend(red(4))
        
        val finalGameBoard = GameBoard(buffer.toList)
	    expectMsg(MoveFeedBack(true, finalGameBoard))
        
        redRest.pollForTurn(testActor)
        expectMsg(TurnFeedBack(false, finalGameBoard))
        
        yellowRest.pollForTurn(testActor)
        expectMsg(TurnFeedBack(false, finalGameBoard))
      }
    }
    
  }

  private def buildRedAndYellowGateways(browser: TestBrowser) = {
    val gameId = createGame(browser)
    val red = RestGateway.registerPlayer(port, gameId)
    val yellow = RestGateway.registerPlayer(port, gameId)
    (buildRestGateway(gameId, red), buildRestGateway(gameId, yellow))
  }

  private def createGame(browser: TestBrowser): Int = {
    browser.goTo(s"http://localhost:${port}${createGameUrl}")
    browser.pageSource match {
      case GameId(id) => id.toInt
      case _          => fail("createGame did not return a game id")
    }
  }

  private def buildRestGateway(gameId: Int, playerId: String) =
    new DefaultRestGateway(gameId, playerId, port)

}
