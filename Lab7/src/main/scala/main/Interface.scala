package main

import main.players.Player

object Interface {

  private def calculatePoints(player: Player): Int =
    player.fields.sum + player.base


  def chooseMove(): Unit = {
    println("Which field do you want to move?")
  }

  def printInputError(): Unit = {
    println("Wrong input, try again.")
  }

  def printTurn(player: Player): Unit = {
    println("Now it's " + player.name + " turn.")
  }

  def printPoints(players: Array[Player]): Unit = {
    for (player <- players) {
      println(player.name + ": " + calculatePoints(player))
    }
  }

  def printFinish(): Unit = {
    println("Game finished")
  }

  def printWinner(player: Player = null): Unit = {
    if(player == null){
      println("Draw!")
    }else {
      println("The winner is: " + player.name + "!")
    }
  }

  def printStartInfo(): Unit = {
    println("Game started!")
  }

  def drawBoard(players: Array[Player]): Unit = {
    val p1 = players(0)
    val p2 = players(1)
    println(s"${p2.name}: [${p2.base}][${p2.fields.reverse.mkString(" ")}]")
    println(s"${p1.name}: [${p1.fields.mkString(" ")}][${p1.base}]")
  }

}
