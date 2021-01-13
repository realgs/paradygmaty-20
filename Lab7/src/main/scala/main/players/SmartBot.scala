package main.players

import main.Game

import scala.util.Random

class SmartBot(name: String, fieldsNumber: Int, rocksNumbers: Int) extends Player(name, fieldsNumber, rocksNumbers) {

  private var game: Game = null
  private var playerIndex: Int = -1

  def setCurrentGame(currentGame: Game): Unit = {
    game = currentGame
    playerIndex = if(this == game.players(0)) 0 else 1
  }

  def copy: Player = {
    val copied = new SmartBot(name, fieldsNumber, rocksNumbers)
    copied.base = base
    copied.fields = fields.clone()
    copied.setCurrentGame(game)

    copied
  }

  def minimax(depth:Int = 5, currentGameState:Game, currentPlayerIndex:Int, isMax: Boolean = true, alpha: Int, beta: Int): Int = {
    val nextGameState = currentGameState.copy
    val playerToMakeMove = currentGameState.players(currentPlayerIndex)
    val availableMoves = playerToMakeMove.getAvailableMoves
    var bestScore:Int = 0

    if(depth == 0 || !currentGameState.started) {
      return currentGameState.calculateDifference(playerIndex)
    }else if(isMax){
      bestScore = Int.MinValue
      var newAlpha = alpha
        for (move <- availableMoves) {
          val nextPlayerIndex = nextGameState.move(currentPlayerIndex, move)
          val score = minimax(depth - 1, nextGameState, nextPlayerIndex, if (currentPlayerIndex == nextPlayerIndex) isMax else !isMax, newAlpha, beta)

          bestScore = Math.max(bestScore, score)
          newAlpha = Math.max(alpha, score)

          if(beta <= newAlpha) {
            return beta
          }
        }

    }else {
      bestScore = Int.MaxValue
      var newBeta = beta
        for(move <- availableMoves){
          val nextPlayerIndex = nextGameState.move(currentPlayerIndex, move)
          val score = minimax(depth - 1, nextGameState, nextPlayerIndex, if(currentPlayerIndex == nextPlayerIndex) isMax else !isMax, alpha, newBeta)

          bestScore = Math.min(bestScore, score)
          newBeta = Math.min(beta, score)

          if(newBeta <= alpha) {
            return alpha
          }
        }

    }

    bestScore

  }

  def decideMove: Int = {
    val availableMoves = getAvailableMoves
    var bestMove = -1
    var bestScore = -1

    for(move <- availableMoves){
      val copiedGame = game.copy
      copiedGame.move(playerIndex, move)
      val score = minimax(5, copiedGame, (playerIndex + 1) % game.numberOfPlayers, false, Int.MinValue, Int.MaxValue)

      println("Move: " + move + ", score: " + score)
      if(bestMove == -1 || score > bestScore) {
        bestMove = move
        bestScore = score
      }else if(score == bestScore){
        val rand = new Random()
        if(rand.nextBoolean()){
          bestMove = move
          bestScore = score
        }
      }
    }

    bestMove
  }
}
