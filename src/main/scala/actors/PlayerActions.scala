package actors

import gameboard.GameBoard

object PlayerActions {
  case object Timeout
  case class MakeMove(gameBoard: GameBoard)
  case class InvalidMove(invalidIndex: Int)
}
