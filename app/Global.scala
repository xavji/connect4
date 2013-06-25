import akka.actor.ActorRef
import akka.actor.Props

import play.api._
import play.libs.Akka

import actors.GameServer
import controllers.Connect4

object Global extends GlobalSettings {

  var gameServer: ActorRef = _

  override def getControllerInstance[A](controllerClass: Class[A]): A = {
    if (controllerClass == classOf[Connect4])
      new Connect4(gameServer).asInstanceOf[A]
    else
      controllerClass.newInstance()
  }

  override def onStart(app: Application) {
    gameServer = Akka.system.actorOf(Props[GameServer], name = "game-server")
  }

}