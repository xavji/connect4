package actors

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.Props

import model._

class GameServer extends Actor with ActorLogging {

  var games: Map[Int, ActorRef] = Map()
  var currentId = 1
  
  def receive = {
    case CreateGame => 
      val game = context.actorOf(Props[Connect4Game])
      sender ! Game(currentId)
      games = games + (currentId -> game)
      currentId += 1
    case reg @ RegistrationRequest(gameId) => 
      forwardToGameActor(gameId, reg, s"registration for unknown game : $gameId")
    case move @ GameMove(gameId, _, _) => 
      forwardToGameActor(gameId, move, s"move for unknown game[$gameId]: $move")
    case status @ PlayerStatusRequest(gameId, _) => 
      forwardToGameActor(gameId, status, s"status request for unknown game: $gameId")
  }

  private def forwardToGameActor(gameId: Int, message: Any, errorMsg: => String) =
	  games.get(gameId)
	  	.map(_.forward(message))
	  	.getOrElse(log.error(errorMsg))
    
  
}