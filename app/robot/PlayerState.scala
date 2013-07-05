package robot

sealed trait PlayerState
case object WaitingForTurn extends PlayerState
case object WaitingForTurnResponse extends PlayerState
case object Playing extends PlayerState

sealed trait FinalGameState extends PlayerState
case object Won extends FinalGameState
case object Lost extends FinalGameState
case object Drawn extends FinalGameState
