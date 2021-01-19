package main

import main.game.players.Player

import scala.io.StdIn.readInt

object Interface {

  private def calculatePoints(player: Player): Int =
    player.fields.sum + player.base

  def decideMove: String = "Which field do you want to move?"
  def printDecideMove(): Unit = {
    println(decideMove)
  }

  def inputError: String = "Wrong input, try again."
  def printInputError(): Unit = {
    println(inputError)
  }

  def turn(player: Player): String = "Now it's " + player.name + " turn."
  def printTurn(player: Player): Unit = {
    println(turn(player))
  }

  def points(players: Array[Player]): String = {
    var text = ""
    for (player <- players) {
      text = text + "\n" + player.name + ": " + calculatePoints(player)
    }

    text
  }
  def printPoints(players: Array[Player]): Unit = {
    println(points(players))
  }

  def finish: String = "Game finished"
  def printFinish(): Unit = {
    println(finish)
  }

  def stop: String = "Game stopped unexpectedly"
  def printStop(): Unit = {
    println(stop)
  }

  def timeout: String = "Player time to decide passed."
  def printTimeout(): Unit = {
    println(stop)
  }

  def winner(player: Player = null): String = {
    if(player == null){
      "Draw!"
    }else {
      "The winner is: " + player.name + "!"
    }
  }
  def printWinner(player: Player = null): Unit = {
    println(winner(player))
  }

  def startInfo: String = "Game started!"
  def printStartInfo(): Unit = {
    println(startInfo)
  }

  def board(players: Array[Player]):String = {
    val p1 = players(0)
    val p2 = players(1)
    var text = ""
    text += s"${p2.name}: [${p2.base}][${p2.fields.reverse.mkString(" ")}]"
    text += s"\n${p1.name}: [${p1.fields.mkString(" ")}][${p1.base}]"

    text
  }
  def drawBoard(players: Array[Player]): Unit = {
    println(board(players))
  }

  def menu:String = "==============================\nChoose game mode:\n1. Human vs Bot(easy)\n2. Human vs Bot(hard)\n3. Bot(easy) vs Bot(easy)\n4. Bot(hard) vs Bot(easy)\n5. Bot(hard) vs Bot(hard)\n==============================="
  def printMenu(): Unit = {
    println(menu)
  }

  def menuOption: Int = {
    var move = -1
    var correctMove = true

    do {
      correctMove = true

      try {
        move = readInt()
        if(move > 5 || move < 1){
          correctMove = false
        }
      }catch {
        case e: NumberFormatException => {
          correctMove = false
        }
      }

      if(!correctMove) {
        Interface.printInputError()
      }

    }while(!correctMove )

    move
  }

}
