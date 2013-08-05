package controllers

import org.junit.runner.RunWith
import org.scalatest.WordSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.MustMatchers

import play.api.libs.json._
import play.api.mvc.Result
import play.api.test._
import play.api.test.Helpers._

import model._
import _root_.util.Connect4Urls

@RunWith(classOf[JUnitRunner])
class Connect4Spec extends WordSpec
    with MustMatchers
    with AsyncResultMapper
    with Connect4Urls {
  
  import GameJsonRead.gameId

  "createGame GET returns a game JSON" in {
    running(FakeApplication()) {
      val resp = routeAsync(FakeRequest(GET, createGameUrl))
      contentTypeIsJson(resp)
      contentAsString(resp) must be === """{"game":{"id":0}}"""
    }
  }

  "register can be POSTed twice and returns a player id" in {
    runTest { gameId =>
      register(gameId, "RED")
      register(gameId, "YELLOW")
    }
  }

  "register accepts an optional ws parameter" in {
    runTest { gameId =>
      register(gameId, "RED")
      register(gameId, "YELLOW", true)
    }
  }
  
  "register POST returns an error status when a game is full" in {
    runTest { gameId =>
      register(gameId, "RED")
      register(gameId, "YELLOW")
      val resp = registerRequest(gameId)
      checkErrorResponse(resp)
    }
  }

  "status GET shows RED starts the game" in {
    runTest { gameId =>
      val red = register(gameId, "RED")
      val yellow = register(gameId, "YELLOW")

      val redUrl = statusUrl(gameId, red)
      val redStatus = routeAsync(FakeRequest(GET, redUrl))
      
      extract[PlayerStatus](redStatus) must be === PlayerStatus(GameBoard.empty, true)
      
      val yellowUrl = statusUrl(gameId, yellow)
      val yellowStatus = routeAsync(FakeRequest(GET, yellowUrl))
      
      extract[PlayerStatus](yellowStatus) must be === PlayerStatus(GameBoard.empty, false)
    }
  }

  "status GET shows RED has to wait for second player to join" in {
    runTest { gameId =>
      val red = register(gameId, "RED")

      val redUrl = statusUrl(gameId, red)
      
      val redStatus = routeAsync(FakeRequest(GET, redUrl))
      extract[PlayerStatus](redStatus) must be === PlayerStatus(GameBoard.empty, false)
      
      val yellow = register(gameId, "YELLOW")
      
      val redStatus2 = routeAsync(FakeRequest(GET, redUrl))
      extract[PlayerStatus](redStatus2) must be === PlayerStatus(GameBoard.empty, true)
    }
  }
  
  "status GET gives error json when the user id does not exist" in {
    runTest { gameId =>
      val unknowUserStatusUrl = statusUrl(gameId, "unknown_user_id")
      
      val unknowUserStatus = routeAsync(FakeRequest(GET, unknowUserStatusUrl))
      
      checkErrorResponse(unknowUserStatus)
    }
  }
  
  "placepiece POST returns game status when it is a legal move" in {
    runTest { gameId =>
      val red = register(gameId, "RED")
      val yellow = register(gameId, "YELLOW")
      val expectedBoardAfterMove = GameBoard(List(ColouredMove.red(3))).toLines
      
      val redMoveUrl = moveUrl(gameId, red, 3)
      val redMove = routeAsync(FakeRequest(POST, redMoveUrl))
      extract[MoveStatus](redMove) must be === MoveStatus(expectedBoardAfterMove, false)
      
      val yellowUrl = statusUrl(gameId, yellow)
      val yellowStatus = routeAsync(FakeRequest(GET, yellowUrl))
      extract[PlayerStatus](yellowStatus) must be === PlayerStatus(expectedBoardAfterMove, true)
    }
  }

  "placepiece POST returns an error status when it is an illegal move" in {
    runTest { gameId =>
      val red = register(gameId, "RED")
      val yellow = register(gameId, "YELLOW")

      val yellowMoveUrl = moveUrl(gameId, yellow, 3)
      val moveStatus = routeAsync(FakeRequest(POST, yellowMoveUrl))
      
      checkErrorResponse(moveStatus)
    }
  }

  private def contentTypeIsJson(res: Result) {
    contentType(res) must be === Some("application/json")
  }

  private def extract[T : Reads](result: Result): T = {
    contentTypeIsJson(result)
    val json = Json.parse(contentAsString(result))
    json.as[T]
  }

  private def register(id: String, expectedColour: String, withWS: Boolean = false) = {
    val resp = registerRequest(id, withWS)
    contentTypeIsJson(resp)
    verifyRegistration(contentAsString(resp), expectedColour, withWS)
  }

  private def registerRequest(gameId: String, withWS: Boolean = false) =
    routeAsync(FakeRequest(POST, registerUrl(gameId, withWS)))

  private def verifyRegistration(resp: String, colour: String, withWS: Boolean = false) = {
    val reg = Json.parse(resp)
    val col = (reg \ "registration" \ "colour").as[String]
    col must be === colour
    
    if (withWS)
      (reg \ "registration" \ "ws").asOpt[Boolean] must be === Some(true)

    val playerId = (reg \ "registration" \ "playerId").asOpt[String]
    playerId.isDefined must be === true
    playerId.get
  }

  private def runTest(test: String => Unit) {
    running(FakeApplication()) {
      val gameResp = routeAsync(FakeRequest(GET, createGameUrl))
      contentTypeIsJson(gameResp)
      val gId = gameId(contentAsString(gameResp))
      test(gId.toString)
    }
  }

  private def checkErrorResponse(resp: Result) {
    contentTypeIsJson(resp)
    val js = Json.parse(contentAsString(resp)) 
    js must be === Json.obj("error" -> "invalid request")
  }

}

object GameJsonRead {
  def gameId(s: String) = (Json.parse(s) \ "game" \ "id").as[Int]
} 