package util

trait Connect4Urls {
  
  val createGameUrl = "/connect4/game/create"

  def statusUrl(gameId: String, playerId: String) =
    s"/connect4/game/${gameId}/player/${playerId}/status"

  def registerUrl(gameId: String, withWS: Boolean = false) =
    s"/connect4/game/${gameId}/register${if (withWS) "?ws=true" else ""}"
  
  def moveUrl(gameId: String, playerId: String, column: Int) =
    s"/connect4/game/${gameId}/player/${playerId}/placepiece/${column}"
  
  def fullUrl(context: String, httpPort: Int) = s"http://localhost:$httpPort$context"
    
}