package actors

import gameboard.GameBoard

object PlayerActions {
  case class Timeout(gameBoard: GameBoard)
  case class MakeMove(gameBoard: GameBoard)
  case class InvalidMove(invalidIndex: Int)
}
