package robot

import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.ActorRef

import play.api.libs.ws.WS
import play.api.libs.json.JsValue

import util.Connect4Urls
import model._

trait RestGateway {

  def pollForTurn(actor: ActorRef): Unit

  def play(column: Int, actor: ActorRef): Unit

}

class DefaultRestGateway(gameId: Int, playerId: String)(httpPort: Int = 9000) extends RestGateway
    with Connect4Urls {

  private def fullUrl(context: String) = s"http://localhost:$httpPort$context"

  def pollForTurn(actor: ActorRef): Unit = {
    val url = fullUrl(statusUrl(gameId.toString, playerId))
    val resp = WS.url(url).get
    resp.onComplete {
      tryResp =>
        val msg = tryResp.map(resp => toTurnFeedBack(resp.json))
                         .getOrElse(TurnFeedBackFailed)
        actor ! msg
    }
  }

  private def toTurnFeedBack(jsValue: JsValue): PlayerProtocol = {
    val isTurn = (jsValue \ "status" \ "ready").as[Boolean]
    val grid = (jsValue \ "status" \ "grid").as[Seq[String]]
    println(grid)
    TurnFeedBack(isTurn, GameBoard.gridToGameBoard(grid))
  }

  def play(column: Int, actor: ActorRef): Unit = {
    val url = fullUrl(moveUrl(gameId.toString, playerId, column))
    val resp = WS.url(url).post("")
    resp.onComplete {
      tryResp =>
        val msg = tryResp.map(resp => toMoveFeedBack(resp.json))
                         .getOrElse(MoveFeedBackFailed)
        actor ! msg
    }
  }

  private def toMoveFeedBack(jsValue: JsValue): PlayerProtocol = {
    val grid = (jsValue \ "result" \ "grid").as[Seq[String]]
    MoveFeedBack(false, GameBoard.gridToGameBoard(grid))
  }

}
