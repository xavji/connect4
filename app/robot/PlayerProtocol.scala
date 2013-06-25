package robot

import model.GameBoard

sealed trait PlayerProtocol
case object PollForTurn extends PlayerProtocol
case class TurnFeedBack(isTurn: Boolean, board: GameBoard) extends PlayerProtocol
case object TurnFeedBackFailed extends PlayerProtocol
case class MoveFeedBack(winningMove: Boolean, board: GameBoard) extends PlayerProtocol
case object MoveFeedBackFailed extends PlayerProtocol

