package robot

sealed trait PlayerState
case object WaitingForTurn extends PlayerState
case object WaitingForTurnResponse extends PlayerState
case object Playing extends PlayerState
case object Done extends PlayerState
