package model

case class Game(id:Int)

sealed trait GameProtocol

case object CreateGame extends GameProtocol

case class RegistrationRequest(gameId: Int) extends GameProtocol

sealed trait Colour {
  val other: Colour
}

case object RED extends Colour {
  val other = YELLOW
}

case object YELLOW extends Colour {
  val other = RED
}

case class Registration(playerId: String, colour: Colour) extends GameProtocol

case class GameMove(gameId: Int, playerId: String, column: Int) extends GameProtocol

case class InvalidRequest(req: GameProtocol) extends GameProtocol

case class MoveStatus(board: Seq[String], winning: Boolean) extends GameProtocol

case class PlayerStatusRequest(gameId: Int, playerId: String) extends GameProtocol

case class PlayerStatus(board: Seq[String], ready: Boolean, winner: Option[Colour] = None) extends GameProtocol
  
object BoardSerialiser {
  
  def serialise(board: Seq[String]): String = {
    val concat = board.map("\"%s\"".format(_)).mkString(", ")
    s"[$concat]"
  }
  
}
