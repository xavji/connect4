package model

import org.junit.runner.RunWith
import org.scalatest.WordSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.MustMatchers
import play.api.libs.json._
import play.api.libs.iteratee.Iteratee
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.JsNumber

@RunWith(classOf[JUnitRunner])
class GameModelJsonSerialisationSpec extends WordSpec
    with MustMatchers {

  val board = Seq("...", "..Y", "..R")
  val expectedJsBoard = JsArray(board map (JsString(_)))
  
  "Game" should {

    "serialise to JSON" in {
      val jsValue = Json.toJson(Game(123))

      jsValue \ "game" \ "id" must be === JsNumber(123)
    }

    "de-serialise from JSON" in {
      val jsValue = Json.obj("game" -> Json.obj("id" -> 456))
      
      jsValue.as[Game] must be === Game(456)
    }

  }
  
  "PlayerStatus" should {

    "serialise to JSON without a winner" in {
      val jsValue = Json.toJson(PlayerStatus(board, true))

      jsValue \ "status" \ "ready" must be === JsBoolean(true)
      (jsValue \ "status" \ "winner").asOpt[String] must be === None
      jsValue \ "status" \ "grid" must be === expectedJsBoard
    }

    "serialise to JSON with a winner" in {
      val jsValue = Json.toJson(PlayerStatus(board, false, Some(RED)))

      jsValue \ "status" \ "ready" must be === JsBoolean(false)
      jsValue \ "status" \ "winner" must be === JsString("RED")
      jsValue \ "status" \ "grid" must be === expectedJsBoard
    }

    "de-serialise from JSON without a winner" in {
      val status = Json.obj(
        "status" -> Json.obj("ready" -> true,
				        	 "grid" -> Json.toJson(board))
      )
      status.as[PlayerStatus] must be === PlayerStatus(board, true)
    }

    "de-serialise from JSON with a winner" in {
      val status = Json.obj(
        "status" -> Json.obj("ready" -> false,
        					 "winner" -> "RED",
				        	 "grid" -> Json.toJson(board))
      )
      status.as[PlayerStatus] must be === PlayerStatus(board, false, Some(RED))
    }
  }

  "Registration" should {
    
    val playerId = "some.id.123"

    "serialise to JSON without a web socket flag" in {
      val jsValue = Json.toJson(Registration(playerId, RED))

      jsValue \ "registration" \ "playerId" must be === JsString(playerId)
      jsValue \ "registration" \ "colour" must be === JsString("RED")
      (jsValue \ "registration" \ "ws").asOpt[Boolean] must be === None
    }

    "serialise to JSON with a web socket flag" in {
      val jsValue = Json.toJson(Registration(playerId, YELLOW, Some((Iteratee.foreach(println), Enumerator.eof))))

      jsValue \ "registration" \ "playerId" must be === JsString(playerId)
      jsValue \ "registration" \ "colour" must be === JsString("YELLOW")
      (jsValue \ "registration" \ "ws").asOpt[Boolean] must be === Some(true)
    }

  }
  
  "MoveStatus" should {

    "serialise to JSON" in {
      val jsValue = Json.toJson(MoveStatus(board, true))

      jsValue \ "result" \ "grid" must be === expectedJsBoard
      jsValue \ "result" \ "winningMove" must be === JsBoolean(true)
    }
    
  }
  
}