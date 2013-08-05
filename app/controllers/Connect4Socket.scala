package controllers
import scala.concurrent.duration._

import akka.actor.ActorRef
import akka.util.Timeout

import play.api._
import play.api.libs._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.iteratee._
import play.api.libs.json.JsString
import play.api.libs.json.JsValue
import play.api.mvc._

import model._

class Connect4Socket(gameServer: ActorRef) extends Controller {

  implicit val timeOut = Timeout(10 seconds)

  def index = WebSocket.using[JsValue] { request => 
    val in = Iteratee.foreach[JsValue](m => println(s"server got $m")).mapDone { m =>
      println("closed")
    }
    
    val out = Enumerator[JsValue](JsString("Hello!"), JsString("Jello>"))
    
    (in, out)
  }

}