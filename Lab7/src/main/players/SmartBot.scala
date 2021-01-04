package main.players

import main.Game

class SmartBot(name: String, fieldsNumber: Int, rocksNumbers: Int) extends Player(name, fieldsNumber, rocksNumbers) {

  private var game: Game = null

  def setCurrentGame(currentGame: Game): Unit = {
    game = currentGame
  }

  def decideMove: Int = {
    println(game)
    0
  }
}
