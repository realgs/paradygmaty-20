package Kalaha

import akka.actor.{ActorSystem, Props}
import scala.io.StdIn.readLine

object Main extends App {

  printInstruction()
  printMenu()
  var number: Int = chooseOption()

  if (number == 1) choice1()
  else if(number == 2) choice2()

  def choice1(): Unit = {
    println("You chose option number 1: ")
    val system = ActorSystem("Kalaha")
    val server = system.actorOf(Props[Server], "Server")
    val computerPlayer = system.actorOf(Props(new ComputerPlayer(server)), "Player1")
    val computerPlayer2 = system.actorOf(Props(new ComputerPlayer(server)), "Player2")
  }

  def choice2(): Unit = {
    println("You chose option number 2: ")
    val system = ActorSystem("Kalaha")
    val server = system.actorOf(Props[Server], "Server")
    val computerPlayer = system.actorOf(Props(new ComputerPlayer(server)), "Player1")
    val humanPlayer = system.actorOf(Props(new Human(server)), "Player2")
  }

  def printMenu(): Unit = {
    println("Menu: ")
    println("[1] - computer Vs computer")
    println("[2] - player Vs computer")
    println("[other] - end")
  }

  def printInstruction(): Unit = {
    println("Welcome in Kalaha game!")
    val instructionBoard = new Board(0)
    for(x <- 0 to 13) {
      instructionBoard.board(x) = x
    }
    instructionBoard.printBoard

    println("Above there is board, which shows the index of holes. Holes from 0 to 5 are for player 1, and his main base " +
      "is on 6, but holes from 7 to 12 are belong to player 2, and his main base is on 13. When the game will start the holes" +
      " will show number of stones in that hole.")
  }

  def chooseOption(): Int = {
    val choice = readLine("Select mode of game: ")
    choice.toInt
  }

}
