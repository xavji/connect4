package actors

import java.util.UUID
import akka.actor.Actor
import akka.actor.FSM

import play.api.libs.iteratee._
import play.api.libs.iteratee.Concurrent.Channel
import play.api.libs.json._

import model._

class Connect4Game(gameId: Int) extends Actor with FSM[GameState, GameBoard] {

  startWith(Registering(RED), GameBoard())

  when(Registering(RED))(redRegistrationHandler orElse defaultHandler)
  when(Registering(YELLOW))(yellowRegistrationHandler orElse defaultHandler)

  when(AwaitingMove(RED))(moveAndStatusHandler orElse defaultHandler)
  when(AwaitingMove(YELLOW))(moveAndStatusHandler orElse defaultHandler)

  when(GameWon(RED))(gameOverHandler orElse defaultHandler)
  when(GameWon(YELLOW))(gameOverHandler orElse defaultHandler)
  when(GameDrawn)(gameOverHandler orElse defaultHandler)

  initialize

  type GameEventHander = PartialFunction[Event, FSM.State[GameState, GameBoard]]

  var redPlayerId: Option[String] = None
  var yellowPlayerId: Option[String] = None
  var redChannel: Option[Channel[JsValue]] = None
  var yellowChannel: Option[Channel[JsValue]] = None

  private val invalidMoveJsValue = Json.obj("type" -> "error", "cause" -> "invalid move")
  
  private def defaultHandler = gameStateHandler orElse invalidRequestHandler

  private def redRegistrationHandler: GameEventHander = {
    case Event(reg @ RegistrationRequest(_, withSockets), _) =>
      redPlayerId = uuid
      val webSocketIteratees =
        if (withSockets) {
          val (iterEnumCouple, channel) = wsIteratees(redPlayerId.get)
          redChannel = Some(channel)
          Some(iterEnumCouple)
        } else None 
      sender ! Registration(redPlayerId.get, RED, webSocketIteratees)
      goto(Registering(YELLOW))
  }

  private def wsIteratees(playerId: String): ((Iteratee[JsValue, Unit], Enumerator[JsValue]), Channel[JsValue]) = {
    val iteratee = Iteratee.foreach[JsValue](jsValue => self ! GameMove(gameId, playerId, jsValue.as[Int]))
    val (enumerator, broadCaster) = Concurrent.broadcast[JsValue]
    ((iteratee, enumerator), broadCaster)
  }

  private def yellowRegistrationHandler: GameEventHander = {
    case Event(reg @ RegistrationRequest(_, withSockets), _) =>
      yellowPlayerId = uuid
      val webSocketIteratees =
        if (withSockets) {
          val (iterEnumCouple, channel) = wsIteratees(yellowPlayerId.get)
          yellowChannel = Some(channel)
          Some(iterEnumCouple)
        } else None
      sender ! Registration(yellowPlayerId.get, YELLOW, webSocketIteratees)
      goto(AwaitingMove(RED))
    case Event(req @ PlayerStatusRequest(gameId, playerId), board) if (isValidPlayerById(playerId)) =>
      sender ! PlayerStatus(board.toLines, false)
      stay
  }

  private def moveAndStatusHandler: GameEventHander = {
    case Event(move @ GameMove(gameId, playerId, column), board) if isValidMove(move, board) =>
      val colour = moveColour(playerId)
      val newmove = ColouredMove(move.column, colour)
      if (board.canPlay(newmove)) {
        val newboard = GameBoard(newmove :: board.moves)
        sendStatusUpdate(newmove, newboard)
        val nextState = gameStateAfterMove(newmove, newboard)
        goto(nextState) using newboard
      } else {
        if (sender != self)
          sender ! InvalidRequest(move)
        getColouredChannel(colour).foreach(_.push(invalidMoveJsValue))
        stay
      }
    case Event(req @ PlayerStatusRequest(gameId, playerId), board) if (isValidPlayerById(playerId)) =>
      sender ! PlayerStatus(board.toLines, isPlayerTurn(playerId, board))
      stay
  }

  private def invalidRequestHandler: GameEventHander = {
    case Event(req: GameProtocolRequest, _) =>
      sender ! InvalidRequest(req)
      stay
  }

  private def gameOverHandler: GameEventHander = {
    case Event(req @ PlayerStatusRequest(gameId, playerId), board) if (isValidPlayerById(playerId)) =>
      sender ! PlayerStatus(board.toLines, false, winnerColour)
      stay
  }

  private def gameStateHandler: GameEventHander = {
    case Event(GameStatusRequest, _) =>
      sender ! GameStatus(gameId, stateName)
      stay
  }

  private def isGameWon = stateName.isInstanceOf[GameWon]

  private def isGameOver = stateName == GameDrawn || isGameWon

  private def uuid = Some(UUID.randomUUID().toString())

  private def isValidMove(move: GameMove, board: GameBoard): Boolean =
    !isGameOver &&
      redPlayerId.isDefined && yellowPlayerId.isDefined &&
      isValidPlayer(move) && isPlayerTurn(move.playerId, board)

  private def isValidPlayer(move: GameMove): Boolean =
    isValidPlayerById(move.playerId)

  private def isValidPlayerById(playerId: String): Boolean =
    redPlayerId.get == playerId || yellowPlayerId.get == playerId

  private def isPlayerTurn(playerId: String, board: GameBoard): Boolean =
    board.moves.headOption
      .map(!_.colour.eq(moveColour(playerId)))
      .getOrElse(playerId == redPlayerId.get)

  private def moveColour(playerId: String) =
    if (redPlayerId.get == playerId) RED
    else YELLOW

  private def winnerColour =
    if (isGameWon) Some(stateData.moves.head.colour)
    else None

  private def sendStatusUpdate(newmove: ColouredMove, newboard: GameBoard) {
    if (sender != self)
      sender ! MoveStatus(newboard.toLines, newboard.isGameWon)
    val isRedTurn = newmove.colour == YELLOW
    redChannel.foreach(_.push(Json.toJson(PlayerStatus(newboard.toLines, isRedTurn))))
    yellowChannel.foreach(_.push(Json.toJson(PlayerStatus(newboard.toLines, !isRedTurn))))
  }

  private def gameStateAfterMove(newmove: ColouredMove, newboard: GameBoard): GameState =
    if (newboard.isGameWon) GameWon(newmove.colour)
    else if (newboard.isGameFull) GameDrawn
    else AwaitingMove(newmove.colour.other)

  private def getColouredChannel(colour: Colour): Option[Channel[JsValue]] =
    if (colour == RED) redChannel
    else yellowChannel

}