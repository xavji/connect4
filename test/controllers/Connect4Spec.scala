package controllers

import play.api.libs.json.Json
import play.api.mvc.Result
import play.api.test._
import play.api.test.Helpers._

import util.Connect4Urls
import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers

class Connect4Spec extends WordSpec
    with MustMatchers
    with AsyncResultMapper
    with Connect4Urls {

  val GameId = """\{ "game": \{ "id": (\d+) \} \}""".r

  "createGame GET returns a game JSON" in {
    running(FakeApplication()) {
      val resp = routeAsync(FakeRequest(GET, createGameUrl))
      contentTypeIsJson(resp)
      contentAsString(resp) must be === """{ "game": { "id": 1 } }"""
    }
  }

  "register can be POSTed twice and returns a player id" in {
    runTest { gameId =>
      register(gameId, "RED")
      register(gameId, "YELLOW")
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
      contentTypeIsJson(redStatus)
      contentAsString(redStatus) must be === """{ "status": { "grid": [".......", ".......", ".......", ".......", ".......", "......."], "ready": true, "winner": "" } }"""

      val yellowUrl = statusUrl(gameId, yellow)
      val yellowStatus = routeAsync(FakeRequest(GET, yellowUrl))
      contentAsString(yellowStatus) must be === """{ "status": { "grid": [".......", ".......", ".......", ".......", ".......", "......."], "ready": false, "winner": "" } }"""
    }
  }

  "placepiece POST returns game status when it is a legal move" in {
    runTest { gameId =>
      val red = register(gameId, "RED")
      val yellow = register(gameId, "YELLOW")

      val redMoveUrl = moveUrl(gameId, red, 3)
      val moveStatus = routeAsync(FakeRequest(POST, redMoveUrl))
      contentTypeIsJson(moveStatus)
      contentAsString(moveStatus) must be === """{ "result": { "grid": [".......", ".......", ".......", ".......", ".......", "..R...."], "winningMove": false } }"""
        
      val yellowUrl = statusUrl(gameId, yellow)
      val yellowStatus = routeAsync(FakeRequest(GET, yellowUrl))
      contentAsString(yellowStatus) must be === """{ "status": { "grid": [".......", ".......", ".......", ".......", ".......", "..R...."], "ready": true, "winner": "" } }"""
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

  private def register(id: String, expectedColour: String) = {
    val resp = registerRequest(id)
    contentTypeIsJson(resp)
    verifyRegistration(contentAsString(resp), expectedColour)
  }

  private def registerRequest(gameId: String) =
    routeAsync(FakeRequest(POST, registerUrl(gameId)))

  private def verifyRegistration(resp: String, colour: String) = {
    val reg = Json.parse(resp)
    val col = (reg \ "registration" \ "colour").as[String]
    col must be === colour

    val playerId = (reg \ "registration" \ "playerId").asOpt[String]
    playerId.isDefined must be === true
    playerId.get
  }

  private def runTest(test: String => Unit) {
    running(FakeApplication()) {
      val gameResp = routeAsync(FakeRequest(GET, createGameUrl))
      contentTypeIsJson(gameResp)
      contentAsString(gameResp) match {
        case GameId(id) => test(id)
        case _          => fail("createGame did not return a game id")
      }
    }
  }

  private def checkErrorResponse(resp: Result) {
    contentTypeIsJson(resp)
    contentAsString(resp) must be === """{ "error": "invalid request" }"""
  }

}