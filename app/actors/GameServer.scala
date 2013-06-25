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
      games.get(gameId)
      	.map(_.forward(reg))
      	.getOrElse(log.error("registration for unknown game : %d", gameId))
    case move @ GameMove(gameId, _, _) => 
      games.get(gameId)
      	.map(_.forward(move))
      	.getOrElse(log.error("move for unknown game[%d]: %s", gameId, move))
    case status @ PlayerStatusRequest(gameId, _) => 
      games.get(gameId)
      	.map(_.forward(status))
      	.getOrElse(log.error("status request for unknown game: %d", gameId))
  }
  
}