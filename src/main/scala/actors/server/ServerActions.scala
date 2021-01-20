package actors.server

object ServerActions {

  case class ConnectToServer(playerId: Int)

  case class ValidateMove(holeIndex: Int)

  case object NextMove

  case object GameOver
}
