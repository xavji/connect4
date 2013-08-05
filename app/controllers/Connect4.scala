package controllers

import scala.concurrent.duration._
import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import play.api._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import play.api.mvc._
import model._
import play.api.libs.json.Json
import play.api.libs.json.Writes

class Connect4(gameServer: ActorRef) extends Controller {

  implicit val timeOut = Timeout(10 seconds)

  val invalidRequest = Ok("""{ "error": "invalid request" }""").as(JSON)
    
  def ping = Action {
    Ok(Json.obj("application" -> "connect4"))
  }

  def createGame = Action {
    val game = gameServer ? CreateGame
    Async {
      game map {
        case g: Game => jsonResponse(g)
      }
    }
  }

  private def jsonResponse[T : Writes](t: T) = Ok(Json.toJson(t))

  def register(gameId: Int, ws: Boolean = false) = Action {
    val regResp = gameServer ? RegistrationRequest(gameId, ws)
    Async {
      regResp map {
        case reg: Registration => jsonResponse(reg)
        case r: InvalidRequest => invalidRequest
      }
    }
  }

  def status(gameId: Int, playerId: String) = Action {
    val statusResp = gameServer ? PlayerStatusRequest(gameId, playerId)
    Async {
      statusResp map {
        case status: PlayerStatus => jsonResponse(status)
        case r: InvalidRequest    => invalidRequest
      }
    }
  }

  def move(gameId: Int, playerId: String, column: Int) = Action {
    val moveResp = gameServer ? GameMove(gameId, playerId, column)
    Async {
      moveResp map {
        case status: MoveStatus => jsonResponse(status)
        case r: InvalidRequest  => invalidRequest
      }
    }
  }

}