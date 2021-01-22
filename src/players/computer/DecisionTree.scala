package pl.pawelklecha.kalaha
package players.computer

import players.Player
import server.Gameboard

import scala.util.control.Breaks.{break, breakable}

class DecisionTree(private val maximizingPlayer: Player.PlayerType) {

  private var minimizingPlayerIndex: Int = _
  private var maximizingPlayerIndex: Int = _

  maximizingPlayer match {
    case Player.PLAYER_1 =>
      minimizingPlayerIndex = Gameboard.PLAYER2_FIRST_INDEX
      maximizingPlayerIndex = Gameboard.PLAYER1_FIRST_INDEX
    case Player.PLAYER_2 =>
      minimizingPlayerIndex = Gameboard.PLAYER1_FIRST_INDEX
      maximizingPlayerIndex = Gameboard.PLAYER2_FIRST_INDEX
  }

  // https://www.geeksforgeeks.org/minimax-algorithm-in-game-theory-set-4-alpha-beta-pruning/
  def minimax(gameboard: Gameboard, depth: Int, isMaximizingPlayer: Boolean, alpha: Int, beta: Int): Int = {
    if (depth == 0) {
      return gameboard.calculateAdvantage(maximizingPlayer)
    }
    if (gameboard.checkEndGameStatus()) {
      return gameboard.calculateAdvantage(maximizingPlayer)
    }

    if (isMaximizingPlayer) {
      var alphaTemp = alpha
      var maxScore = Int.MinValue

      breakable {
        for (holeIndex <- maximizingPlayerIndex until maximizingPlayerIndex + Gameboard.GAMEBOARD_SIZE) {
          if (gameboard.checkMoveCorrectness(holeIndex)) {
            val score = goDeeper(
              gameboard = gameboard.clone(),
              depth = depth - 1,
              holeIndex = holeIndex,
              alpha = alphaTemp,
              beta = beta
            )

            maxScore = math.max(maxScore, score)
            alphaTemp = math.max(alphaTemp, score)

            if (beta <= alphaTemp) {
              break
            }
          }
        }
      }

      maxScore
    }
    else {
      var betaTemp = beta
      var minScore = Int.MaxValue

      breakable {
        for (holeIndex <- minimizingPlayerIndex until minimizingPlayerIndex + Gameboard.GAMEBOARD_SIZE) {
          if (gameboard.checkMoveCorrectness(holeIndex)) {
            val score = goDeeper(
              gameboard = gameboard.clone(),
              depth = depth - 1,
              holeIndex = holeIndex,
              alpha = alpha,
              beta = betaTemp
            )

            minScore = math.min(minScore, score)
            betaTemp = math.min(betaTemp, score)

            if (betaTemp <= alpha) {
              break
            }
          }
        }
      }

      minScore
    }
  }

  private def goDeeper(gameboard: Gameboard, depth: Int, holeIndex: Int, alpha: Int, beta: Int): Int = {
    gameboard.makeMove(holeIndex)

    minimax(
      gameboard = gameboard,
      depth = depth - 1,
      alpha = alpha,
      beta = beta,
      isMaximizingPlayer = gameboard.getCurrentPlayer == maximizingPlayer
    )
  }
}