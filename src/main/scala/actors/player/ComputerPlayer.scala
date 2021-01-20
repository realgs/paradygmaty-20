package actors.player

import actors.player.ComputerPlayer.DECISION_TREE_SEARCH_DEPTH
import actors.server.ServerActions
import akka.actor.{Actor, ActorRef, Props}
import decisiontree.DecisionTree
import gameboard.GameBoard
import model.{GameConstants, Player}

class ComputerPlayer(server: ActorRef, player: Player.Value) extends Actor {

  private val decisionTree = new DecisionTree(player)
  private var firstHoleIndex: Int = _

  player match {
    case Player.First => firstHoleIndex = GameConstants.PLAYER_ONE_FIRST_HOLE_INDEX
    case Player.Second => firstHoleIndex = GameConstants.PLAYER_TWO_FIRST_HOLE_INDEX
  }

  server ! ServerActions.ConnectToServer(player.id)

  override def receive: Receive = {
    case PlayerActions.MakeMove(gameBoard: GameBoard) => {
      sender ! getNextMove(gameBoard)
    }
  }

  private def getNextMove(gameBoard: GameBoard): Int = {
    var bestMove = firstHoleIndex
    var bestScore = Int.MinValue

    for (holeIndex <- firstHoleIndex until firstHoleIndex + GameConstants.PLAYERS_HOLES_NUMBER) {
      if (gameBoard.isMoveValid(holeIndex)) {
        val newBoard = gameBoard.clone()
        newBoard.makeMove(holeIndex)

        val score = decisionTree.minimax(
          gameBoard = newBoard,
          depth = DECISION_TREE_SEARCH_DEPTH,
          alpha = Int.MinValue,
          beta = Int.MaxValue,
          isMaximizingPlayer = newBoard.getActualTurn == player
        )

        if (score > bestScore) {
          bestScore = score
          bestMove = holeIndex
        }
      }
    }

    bestMove
  }
}

object ComputerPlayer {
  def props(server: ActorRef, player: Player.Value): Props = Props(classOf[ComputerPlayer], server, player)

  private val DECISION_TREE_SEARCH_DEPTH = 10
}
