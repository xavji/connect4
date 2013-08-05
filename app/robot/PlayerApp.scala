package robot

import java.util.concurrent.CountDownLatch

import scala.concurrent.duration._

import akka.actor._

import model.MonteCarloSolver

object PlayerApp extends App {

  val gameId = args(0).toInt
  val playouts = args(1).toInt
  val searchDepth = args(2).toInt

  val playerId = RestGateway.registerPlayer(9000, gameId)
  val latch = new CountDownLatch(1)

  println(s"registered $playerId on game $gameId")

  implicit val actorSystem = ActorSystem("connect4-robot")

  val player = actorSystem.actorOf(Props(new PollingPlayerActor(new DefaultRestGateway(gameId, playerId), new MonteCarloSolver(playouts, searchDepth), latch)), "player")

  import scala.concurrent.ExecutionContext.Implicits.global

  actorSystem.scheduler.schedule(
    0 milliseconds,
    200 milliseconds,
    player,
    PollForTurn
  )

  latch.await()
  actorSystem.shutdown
  actorSystem.awaitTermination(2 seconds)
  System.exit(0)
}

