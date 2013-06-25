package util

trait Connect4Urls {
  
  val createGameUrl = "/connect4/game/create"

  def statusUrl(gameId: String, playerId: String) =
    s"/connect4/game/${gameId}/player/${playerId}/status"

  def registerUrl(gameId: String) =
    s"/connect4/game/${gameId}/register"
  
  def moveUrl(gameId: String, playerId: String, column: Int) =
    s"/connect4/game/${gameId}/player/${playerId}/placepiece/${column}"
  
}