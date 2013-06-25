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

class Connect4(gameServer: ActorRef) extends Controller {

  implicit val timeOut = Timeout(10 seconds)

  val invalidRequest = """{ "error": "invalid request" }"""
    
  def ping = Action {
    Ok(JsObject(Seq("application" -> JsString("connect4"))))
  }

  def createGame = Action {
    val game = gameServer ? CreateGame
    Async {
      game map {
        case Game(id) =>
          val json = s"""{ "game": { "id": ${id} } }"""
          jsonResponse(json)
      }
    }
  }

  private def jsonResponse(s: String) = Ok(s).as(JSON)

  def register(gameId: Int) = Action {
    val regResp = gameServer ? RegistrationRequest(gameId)
    Async {
      regResp map {
        case reg: Registration =>
          val json = s"""{ "registration": { "playerId": "${reg.playerId}", "colour": "${reg.colour}" } }"""
          jsonResponse(json)
        case r: InvalidRequest  => jsonResponse(invalidRequest)
      }
    }
  }

  def status(gameId: Int, playerId: String) = Action {
    val statusResp = gameServer ? PlayerStatusRequest(gameId, playerId)
    Async {
      statusResp map {
        case status: PlayerStatus =>
          val grid = BoardSerialiser.serialise(status.board)
          val json = s"""{ "status": { "grid": $grid, "ready": ${status.ready}, "winner": "${status.winner.getOrElse("")}" } }"""
          jsonResponse(json)
      }
    }
  }

  def move(gameId: Int, playerId: String, column: Int) = Action {
    val moveResp = gameServer ? GameMove(gameId, playerId, column)
    Async {
      moveResp map {
        case status: MoveStatus =>
          val grid = BoardSerialiser.serialise(status.board)
          val json = s"""{ "result": { "grid": $grid, "winningMove": ${status.winning} } }"""
          jsonResponse(json)
        case r: InvalidRequest  => jsonResponse(invalidRequest)
      }
    }
  }

}