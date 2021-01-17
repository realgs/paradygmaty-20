package actors.server

object ServerActions {

  case class ConnectToServer(playerId: Int)

  case object StartGame

  case class ValidateMove(holeIndex: Int)

  case object NextMove

  case object GameOver

  case object Timeout

  case object EndGame

}
