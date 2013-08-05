package actors

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._

import akka.actor._
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import model._

class GameServer extends Actor with ActorLogging {

  val games = ListBuffer[ActorRef]()
  
  implicit val timeOut = Timeout(10 seconds)
  
  def receive = {
    case CreateGame => 
      val currentId = games.size
      val game = context.actorOf(Props(new Connect4Game(currentId)))
      sender ! Game(currentId)
      games += game
    case reg @ RegistrationRequest(gameId, _) => 
      forwardToGameActor(gameId, reg, s"registration for unknown game: $gameId")
    case move @ GameMove(gameId, _, _) => 
      forwardToGameActor(gameId, move, s"move for unknown game[$gameId]: $move")
    case status @ PlayerStatusRequest(gameId, _) => 
      forwardToGameActor(gameId, status, s"status request for unknown game: $gameId")
    case ListGames =>
      val statuses = games map {
        actor => (actor ? GameStatusRequest).mapTo[GameStatus]
      }
      Future.sequence(statuses) map (ss => ListGamesResponse(ss.toList)) pipeTo sender
  }

  private def forwardToGameActor(gameId: Int, message: Any, errorMsg: => String) =
    if (games.isDefinedAt(gameId))
      games(gameId).forward(message)
    else
	  log.error(errorMsg)
    
}