package model

import play.api.libs.iteratee.Iteratee
import play.api.libs.iteratee.Enumerator
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.data.validation.ValidationError


case class Game(id: Int)

object Game {

  implicit val gameWrites = new Writes[Game] {
    def writes(o: Game): JsValue = Json.obj("game" -> Json.obj("id" -> o.id))
  }

  implicit val gameReads: Reads[Game] = 
    (JsPath \ "game" \ "id").read[Int].map(id => Game(id))
  
}

sealed trait GameProtocolRequest
sealed trait GameProtocolResponse

case object CreateGame extends GameProtocolRequest

case class RegistrationRequest(gameId: Int, withWebSocket: Boolean = false) extends GameProtocolRequest

sealed trait Colour {
  val other: Colour
}

case object RED extends Colour {
  val other = YELLOW
}

case object YELLOW extends Colour {
  val other = RED
}

object Colour {
  def apply(col: String): Colour =
    if ("RED" == col) RED
    else if ("YELLOW" ==col) YELLOW
    else throw new RuntimeException("invalid colour " + col) 
}

case class Registration(playerId: String, colour: Colour, wsSocketIteratees: Option[(Iteratee[JsValue, Unit], Enumerator[JsValue])] = None) extends GameProtocolResponse

object Registration {

  implicit val registrationWrites = new Writes[Registration] {
    def writes(o: Registration): JsValue = {
      val reg = Json.obj(
        "playerId" -> o.playerId,
        "colour" -> o.colour.toString
      )
      val wsReg = o.wsSocketIteratees map { ws => reg + ("ws" -> JsBoolean(true)) } getOrElse reg
      Json.obj("registration" -> wsReg)
    }
  }

}

case class GameMove(gameId: Int, playerId: String, column: Int) extends GameProtocolRequest

case class InvalidRequest(req: GameProtocolRequest) extends GameProtocolResponse

case class MoveStatus(board: Seq[String], winning: Boolean) extends GameProtocolResponse

object MoveStatus {

  implicit val moveStatusWrites = new Writes[MoveStatus] {
    def writes(o: MoveStatus): JsValue =
      Json.obj(
        "result" -> Json.obj(
          "winningMove" -> o.winning,
          "grid" -> Json.toJson(o.board)
        )
      )
  }

  implicit val moveStatusReads: Reads[MoveStatus] = {
    val reader = (
      (__ \ "result" \ "grid").read[Seq[String]] and 
      (__ \ "result" \ "winningMove").read[Boolean] 
    )
    val constructor: ((Seq[String], Boolean)) => MoveStatus = 
      couple => MoveStatus(couple._1, couple._2)
    reader.tupled map constructor
  }
  
}

case class PlayerStatusRequest(gameId: Int, playerId: String) extends GameProtocolRequest

case class PlayerStatus(board: Seq[String], ready: Boolean, winner: Option[Colour] = None) extends GameProtocolResponse

object PlayerStatus {

  implicit val playerStatusWrites = new Writes[PlayerStatus] {
    def writes(o: PlayerStatus): JsValue = {
      val status = Json.obj(
        "ready" -> o.ready,
        "grid" -> Json.toJson(o.board)
      )
      val winnerStatus = o.winner map { col => status + ("winner" -> JsString(col.toString())) } getOrElse status
      Json.obj("status" -> winnerStatus)
    }
  }
  
  implicit val playerStatusReads: Reads[PlayerStatus] = {
    val reader = (
      (__ \ "status" \ "grid").read[Seq[String]] and 
      (__ \ "status" \ "ready").read[Boolean] and 
      (__ \ "status" \ "winner").readNullable[String]
    )
    val constructor: ((Seq[String], Boolean, Option[String])) => PlayerStatus = 
      triplet => PlayerStatus(triplet._1, triplet._2, triplet._3 map (Colour(_)))
    reader.tupled map constructor
  }

}

case object ListGames extends GameProtocolRequest

case class ListGamesResponse(statuses: List[GameStatus]) extends GameProtocolResponse

case object GameStatusRequest extends GameProtocolRequest
case class GameStatus(gameId: Int, state: GameState) extends GameProtocolResponse

sealed trait GameState
case class Registering(colour: Colour) extends GameState
case class AwaitingMove(colour: Colour) extends GameState
case object GameDrawn extends GameState
case class GameWon(colour: Colour) extends GameState

object BoardSerialiser {

  def serialise(board: Seq[String]): String = {
    val concat = board.map("\"%s\"".format(_)).mkString(", ")
    s"[$concat]"
  }

}
