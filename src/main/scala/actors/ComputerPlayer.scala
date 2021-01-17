package actors

import actors.ComputerPlayer.DECISION_TREE_SEARCH_DEPTH
import akka.actor.{Actor, ActorRef, Props}
import decisiontree.DecisionTree
import gameboard.GameBoard
import model.{GameConstants, Player}

import scala.util.Random

class ComputerPlayer(server: ActorRef, player: Player.Value) extends Actor {

  private val decisionTree = new DecisionTree(player)
  private var firstHoleIndex: Int = _

  player match {
    case Player.First => firstHoleIndex = GameConstants.PLAYER_ONE_FIRST_HOLE_INDEX
    case Player.Second => firstHoleIndex = GameConstants.PLAYER_TWO_FIRST_HOLE_INDEX
  }

  server ! Server.ConnectToServer

  override def receive: PartialFunction[Any, Unit] = {
    case PlayerActions.MakeMove(gameBoard: GameBoard) => {
      val bestMove = getNextMove(gameBoard)

      server ! Server.ValidateMove(bestMove)
    }

    case PlayerActions.Timeout(gameBoard: GameBoard) => {
      println("Koniec czasu")
      var randomMove = 0

      do {
        randomMove = Random.between(firstHoleIndex, firstHoleIndex + GameConstants.PLAYERS_HOLES_NUMBER)
      } while (gameBoard.isMoveValid(randomMove))

      server ! Server.ValidateMove(randomMove)
    }

  }

  private def getNextMove(gameBoard: GameBoard): Int = {
    var bestMove = firstHoleIndex
    var bestScore = Int.MinValue
    val alpha = Int.MinValue
    val beta = Int.MaxValue

    for (holeIndex <- firstHoleIndex until firstHoleIndex + GameConstants.PLAYERS_HOLES_NUMBER) {
      if (gameBoard.isMoveValid(holeIndex)) {
        val newBoard = gameBoard.clone()
        newBoard.makeMove(holeIndex)

        val score = decisionTree.minimax(newBoard, DECISION_TREE_SEARCH_DEPTH, alpha, beta, newBoard.getActualTurn == player)

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

  private val DECISION_TREE_SEARCH_DEPTH = 8
}
