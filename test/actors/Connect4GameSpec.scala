package actors
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration._

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.testkit.ImplicitSender
import akka.testkit.TestKit
import akka.util.Timeout

import org.junit.runner.RunWith
import org.scalatest.WordSpec
import org.scalatest.concurrent.Futures
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.MustMatchers

import play.api.libs.iteratee._
import play.api.libs.json._

import model._
import model.ColouredMove._

@RunWith(classOf[JUnitRunner])
class Connect4GameSpec extends TestKit(ActorSystem("Connect4GameSpecSystem"))
    with WordSpec
    with MustMatchers
    with StopSystemAfterAll
    with ImplicitSender {

  val GAME_ID = 1234
  implicit val timeout = Timeout(5 seconds)

  def c4Game = system.actorOf(Props(new Connect4Game(GAME_ID)))

  "a game with two players registered with websockets" when {

    "receiving a valid move through a socket" should {

      "send a status message with the outcome of the move to each actor's socket" in {
        val game = c4Game
        implicit val registrations = registerPlayers(game)

        val (redStatus, yellowStatus) = playColumn(registrations.red.wsSocketIteratees.get._1, 2)

        val boardLines = GameBoard(List(red(2))).toLines

        extractFuture(redStatus) must be === Json.toJson(PlayerStatus(boardLines, false))
        extractFuture(yellowStatus) must be === Json.toJson(PlayerStatus(boardLines, true))
        
        val (redStatus2, yellowStatus2) = playColumn(registrations.yellow.wsSocketIteratees.get._1, 3)

        val boardLines2 = GameBoard(List(yellow(3), red(2))).toLines

        extractFuture(redStatus2) must be === Json.toJson(PlayerStatus(boardLines2, true))
        extractFuture(yellowStatus2) must be === Json.toJson(PlayerStatus(boardLines2, false))
      }

    }

    "receiving an invalid move through a socket" should {

      "send an error message to the originating actor's socket" in {
        val game = c4Game
        implicit val registrations = registerPlayers(game)

        val (redStatus, yellowStatus) = playColumn(registrations.red.wsSocketIteratees.get._1, -22)

        extractFuture(redStatus) must be === Json.parse("""{"type":"error", "cause":"invalid move"}""")
        expectNoMsg(500 milliseconds)
        yellowStatus.isCompleted must be === false
      }

    }
    
  }

  private def extractFuture(fut: Future[JsValue]): JsValue = Await.result(fut, 2 seconds)
  
  case class Registrations(red: Registration, yellow: Registration)
  
  private def registerPlayers(game: ActorRef): Registrations = {
    game ! RegistrationRequest(GAME_ID, true)
    val redReg = expectMsgClass(classOf[Registration])
    game ! RegistrationRequest(GAME_ID, true)
    val yellowReg = expectMsgClass(classOf[Registration])
    Registrations(redReg, yellowReg)
  }

  private def playColumn(in: Iteratee[JsValue, Unit], column: Int)
  						(implicit registrations: Registrations): (Future[JsValue], Future[JsValue]) = {
    in.feed(Input.El(JsNumber(column)))
    (registrations.red.wsSocketIteratees.get._2.run(firstValue), registrations.yellow.wsSocketIteratees.get._2.run(firstValue))
  }

  def firstValue: Iteratee[JsValue, JsValue] =
    Cont(in =>
      in match {
        case Input.El(el) => {
          Done(el, Input.Empty)
        }
        case Input.EOF => {
          Error("EOF received.", Input.Empty)
        }
        case Input.Empty => {
          Error("Empty received.", Input.Empty)
        }
      })

}