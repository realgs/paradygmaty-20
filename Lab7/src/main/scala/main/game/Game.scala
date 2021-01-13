package main.game

import main.game.players.{Human, Player, SmartBot}

class Game(
            p1: Player = new Human("p1", 6, 6),
            p2: Player = new Human("p2", 6, 6)
          ) {
  assert(p1.fieldsNumber == p2.fieldsNumber)
  val numberOfFields:Int = p1.fieldsNumber
  val numberOfPlayers:Int = 2
  var players:Array[Player] = Array(p1,p2)
  var started:Boolean = false

  if(p1.isInstanceOf[SmartBot]) {
    p1.asInstanceOf[SmartBot].setCurrentGame(this)
  }

  if(p2.isInstanceOf[SmartBot]){
    p2.asInstanceOf[SmartBot].setCurrentGame(this)
  }

  def stealRocksFromOppositeSide(player: Int, field: Int):Unit = {
    players(player).fields(field) = players((player + 1) % numberOfPlayers).fields(numberOfFields - 1 - field)
    players((player + 1) % numberOfPlayers).fields(numberOfFields - 1 - field) = 0
  }

  def move(playerIndex: Int, fieldIndex: Int): Int = {
    assert(playerIndex >= 0 && playerIndex < numberOfPlayers && fieldIndex >= 0 && fieldIndex < numberOfFields)

    var repeatMove = false
    var player = players(playerIndex)
    var rocksLeft = player.fields(fieldIndex)
    player.fields(fieldIndex) = 0

    var currentFieldIndex = fieldIndex
    var currentPlayerIndex = playerIndex

    while(rocksLeft > 0) {
      rocksLeft = rocksLeft - 1
      currentFieldIndex = currentFieldIndex + 1

      // Rock in the base
      if(currentFieldIndex == numberOfFields){
        // Last rock is dropped into the base, player gets another move.
        if(rocksLeft == 0 && currentPlayerIndex == playerIndex){
          repeatMove = true
        }

        player.base = player.base + 1
      }
      // Rock entering other player fields
      else if(currentFieldIndex > numberOfFields) {
        currentFieldIndex = 0
        currentPlayerIndex = (currentPlayerIndex + 1) % numberOfPlayers
        player = players(currentPlayerIndex)
        player.fields(currentFieldIndex) = player.fields(currentFieldIndex) + 1
      }
      // Standard rock in the field
      else {
        // Last rock is dropped into empty field owned by a player who made a move
        if(rocksLeft == 0 && currentPlayerIndex == playerIndex && player.fields(currentFieldIndex) == 0){
          stealRocksFromOppositeSide(currentPlayerIndex, currentFieldIndex)
        }

        player.fields(currentFieldIndex) = player.fields(currentFieldIndex) + 1
      }
    }

    if(isFinished){
      finish()
    }

    if(repeatMove){
      playerIndex
    }else {
      (playerIndex + 1) % numberOfPlayers
    }
  }

  def calculatePoints(player: Player): Int =
    player.fields.sum + player.base

  def calculateDifference(index: Int = 0):Int =
    calculatePoints(players(index)) - calculatePoints(players((index + 1) % numberOfPlayers))

  def isFinished: Boolean =
    calculatePoints(players(0)) == players(0).base || calculatePoints(players(1)) == players(1).base

  def start():Unit = {
    started = true
  }

  def finish(): Unit = {
    started = false
  }

  def winner: Player = {
    var winner: Player = null

    if(!started){
      val p0Points = calculatePoints(players(0))
      val p1Points = calculatePoints(players(1))

      if(p0Points < p1Points) {
        winner = players(1)
      } else if(p0Points > p1Points) {
        winner = players(0)
      }
    }

    winner
  }

  def copy: Game = {
    val game = new Game(players(0).copy, players(1).copy)
    game.started = started
    game
  }

  override def toString = s"Started: $started\nGame main.game.players: \n ${players(0)} \n ${players(1)}"
}
