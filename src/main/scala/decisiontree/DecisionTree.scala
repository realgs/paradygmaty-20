package decisiontree

import gameboard.GameBoard
import model.{GameConstants, Player}

import scala.util.control.Breaks.{break, breakable}

class DecisionTree(private val maximizingPlayer: Player.Value) {
  private var maximizingPlayerFirstIndex: Int = _
  private var minimizingPlayerFirstIndex: Int = _

  maximizingPlayer match {
    case Player.First =>
      maximizingPlayerFirstIndex = GameConstants.PLAYER_ONE_FIRST_HOLE_INDEX
      minimizingPlayerFirstIndex = GameConstants.PLAYER_TWO_FIRST_HOLE_INDEX
    case Player.Second =>
      maximizingPlayerFirstIndex = GameConstants.PLAYER_TWO_FIRST_HOLE_INDEX
      minimizingPlayerFirstIndex = GameConstants.PLAYER_ONE_FIRST_HOLE_INDEX
  }


  def minimax(gameBoard: GameBoard, depth: Int, alpha: Int, beta: Int, isMaximizingPlayer: Boolean): Int = {
    if (gameBoard.isGameOver) {
      //println("Decision Tree - Game Over")
      return gameBoard.getFinalScore(maximizingPlayer)
    }
    if (depth == 0) {
      //println("Decision Tree - depth 0")
      return gameBoard.getActualScore(maximizingPlayer)
    }

    if (isMaximizingPlayer) {
      var alphaLocal = alpha
      var maxScore: Int = Int.MinValue

      breakable {
        for (holeIndex <- maximizingPlayerFirstIndex until maximizingPlayerFirstIndex + GameConstants.PLAYERS_HOLES_NUMBER) {
          if (gameBoard.isMoveValid(holeIndex)) {
            val newBoard = gameBoard.clone()
            newBoard.makeMove(holeIndex)

            val score = minimax(
              gameBoard = newBoard,
              depth = depth - 1,
              alpha = alphaLocal,
              beta = beta,
              isMaximizingPlayer = newBoard.getActualTurn == maximizingPlayer
            )

            maxScore = math.max(maxScore, score)
            alphaLocal = math.max(alphaLocal, score)

            if (beta <= alphaLocal) {
              break
            }
          }
        }
      }

      maxScore
    }
    else {
      var betaLocal = beta
      var minScore: Int = Int.MaxValue

      breakable {
        for (holeIndex <- minimizingPlayerFirstIndex until minimizingPlayerFirstIndex + GameConstants.PLAYERS_HOLES_NUMBER) {
          if (gameBoard.isMoveValid(holeIndex)) {
            val newBoard = gameBoard.clone()
            newBoard.makeMove(holeIndex)

            val score = minimax(
              gameBoard = newBoard,
              depth = depth - 1,
              alpha = alpha,
              beta = betaLocal,
              isMaximizingPlayer = newBoard.getActualTurn == maximizingPlayer
            )

            minScore = math.min(minScore, score)
            betaLocal = math.min(betaLocal, score)

            if (betaLocal <= alpha) {
              break
            }
          }
        }
      }

      minScore
    }
  }
}
