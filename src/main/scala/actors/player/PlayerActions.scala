package actors.player

import gameboard.GameBoard

object PlayerActions {
  case class MakeMove(gameBoard: GameBoard)
}
