package actors

import java.util.UUID

import akka.actor.Actor
import akka.actor.FSM

import model._

sealed trait GameState

case class Registering(colour: Colour) extends GameState

case object AwaitingMove extends GameState

case object GameDrawn extends GameState

case object GameWon extends GameState

class Connect4Game extends Actor with FSM[GameState, GameBoard] {

  startWith(Registering(RED), GameBoard())

  when(Registering(RED))(redRegistrationHandler orElse invalidRequestHandler)

  when(Registering(YELLOW))(yellowRegistrationHandler orElse invalidRequestHandler)

  when(AwaitingMove)(moveAndStatusHandler orElse invalidRequestHandler)

  when(GameWon)(gameOverHandler orElse invalidRequestHandler)
  
  when(GameDrawn)(gameOverHandler orElse invalidRequestHandler)
  
  initialize
  
  type GameEventHander = PartialFunction[Event, FSM.State[GameState, GameBoard]]

  var redPlayerId: Option[String] = None
  var yellowPlayerId: Option[String] = None
  
  private def redRegistrationHandler: GameEventHander = {
    case Event(reg @ RegistrationRequest(_), _) =>
      redPlayerId = uuid
      sender ! Registration(redPlayerId.get, RED)
      goto(Registering(YELLOW))
  }
  
  private def yellowRegistrationHandler: GameEventHander = {
    case Event(reg @ RegistrationRequest(_), _) =>
      yellowPlayerId = uuid
      sender ! Registration(yellowPlayerId.get, YELLOW)
      goto(AwaitingMove)
    case Event(req @ PlayerStatusRequest(gameId, playerId), board) if (isValidPlayerById(playerId)) =>
      sender ! PlayerStatus(board.toLines, false)
      stay
  }
  
  private def moveAndStatusHandler: GameEventHander = {
    case Event(move @ GameMove(gameId, playerId, column), board) if (isValidMove(move, board)) =>
      val newmove = ColouredMove(move.column, moveColour(move.playerId))
      if (board.canPlay(newmove)) {
        val newboard = GameBoard(newmove :: board.moves)
        val gameWon = newboard.isGameWon
        sender ! MoveStatus(newboard.toLines, gameWon)
        val nextState =
          if (gameWon) GameWon
          else if (newboard.isGameFull) GameDrawn
          else AwaitingMove
        goto(nextState) using newboard
      } else {
        sender ! InvalidRequest(move)
        stay
      }
    case Event(req @ PlayerStatusRequest(gameId, playerId), board) if (isValidPlayerById(playerId)) =>
      sender ! PlayerStatus(board.toLines, isPlayerTurn(playerId, board))
      stay
  }
  
  private def invalidRequestHandler: GameEventHander = {
    case Event(req: GameProtocol, _) =>
      sender ! InvalidRequest(req)
      stay
  }
  
  private def gameOverHandler: GameEventHander = {
    case Event(req @ PlayerStatusRequest(gameId, playerId), board) if (isValidPlayerById(playerId)) =>
      sender ! PlayerStatus(board.toLines, false, winnerColour)
      stay
  }
   
  private def isGameWon = stateName == GameWon
  
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

}