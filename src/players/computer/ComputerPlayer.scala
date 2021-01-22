package pl.pawelklecha.kalaha
package players.computer

import events.{PlayerActionEvent, ServerActionEvent}
import players.Player
import players.Player.PlayerType
import players.computer.ComputerPlayer.DECISION_TREE_DEPTH
import server.Gameboard

import akka.actor.Actor

class ComputerPlayer(playerType: PlayerType) extends Actor {

  private val decisionTree = new DecisionTree(playerType)

  override def receive: Receive = {
    case ServerActionEvent.MakeMove(gameboard) => sender ! PlayerActionEvent.ConfirmMove(getNextMove(gameboard))
  }

  private def getNextMove(gameboard: Gameboard): Int = {
    val startingIndex = playerType match {
      case Player.PLAYER_1 => Gameboard.PLAYER1_FIRST_INDEX
      case Player.PLAYER_2 => Gameboard.PLAYER2_FIRST_INDEX
    }
    var bestScore = Int.MinValue
    var bestChoose = startingIndex

    for (currentHole <- startingIndex until startingIndex + Gameboard.GAMEBOARD_SIZE) {
      if (gameboard.checkMoveCorrectness(currentHole)) {
        val gameboardCopy = gameboard.clone()
        gameboardCopy.makeMove(currentHole)

        val score = decisionTree.minimax(
          gameboard = gameboardCopy,
          depth = DECISION_TREE_DEPTH,
          alpha = Int.MinValue,
          beta = Int.MaxValue,
          isMaximizingPlayer = gameboardCopy.getCurrentPlayer == playerType
        )

        if (score > bestScore) {
          bestChoose = currentHole
          bestScore = score
        }
      }
    }
    bestChoose
  }

}

object ComputerPlayer {
  private val DECISION_TREE_DEPTH = 6
}
