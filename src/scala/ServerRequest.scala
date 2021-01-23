package scala

object ServerRequest {
  case object START_GAME
  case object JOIN
  case class TAKE_FROM_HOLE(number: Int, playerNumber: PlayerNumber)
  case object END_GAME
}
