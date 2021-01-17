package decisiontree

import gameboard.GameBoard
import model.{GameConstants, Player}

import scala.util.control.Breaks.break

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
      return gameBoard.getFinalScore(maximizingPlayer)
    }
    if (depth == 0) {
      return gameBoard.getActualScore(maximizingPlayer)
    }

    if (isMaximizingPlayer) {
      var alphaLocal = alpha
      var maxScore: Int = Int.MinValue

      for (holeIndex <- maximizingPlayerFirstIndex until maximizingPlayerFirstIndex + GameConstants.PLAYERS_HOLES_NUMBER) {
        if (gameBoard.isMoveValid(holeIndex)) {
          val newBoard = gameBoard.clone()
          newBoard.makeMove(holeIndex)

          val score = minimax(newBoard, depth - 1, alphaLocal, beta, newBoard.getActualTurn == maximizingPlayer)

          maxScore = math.max(maxScore, score)
          alphaLocal = math.max(alphaLocal, score)

          if (beta <= alphaLocal) {
            break
          }
        }
      }

      maxScore
    }
    else {
      var betaLocal = beta
      var minScore: Int = Int.MaxValue

      for (holeIndex <- minimizingPlayerFirstIndex until minimizingPlayerFirstIndex + GameConstants.PLAYERS_HOLES_NUMBER) {
        if (gameBoard.isMoveValid(holeIndex)) {
          val newBoard = gameBoard.clone()
          newBoard.makeMove(holeIndex)

          val score = minimax(newBoard, depth - 1, alpha, betaLocal, newBoard.getActualTurn == maximizingPlayer)

          minScore = math.min(minScore, score)
          betaLocal = math.min(betaLocal, score)

          if (betaLocal <= alpha) {
            break
          }
        }
      }

      minScore
    }
  }
}
