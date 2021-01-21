import akka.actor.{ActorSystem, Props}

import scala.util.Random

object Game extends App{

  printGameChoiceMenu()
  val usersChoice = getUserInputFromConsole
  if(usersChoice == 1){
    println("You hve chosen to watch Computer vs computer match")
    println("Please choose starting number of seeds in each pit [3-6]")
    var startingSeeds = 0
    while(startingSeeds < 3 || startingSeeds > 6){
      startingSeeds = getUserInputFromConsole
    }
  }
  if(usersChoice == 2){
    println("You hve chosen to play against vs computer")
    println("Please choose starting number of seeds in each pit [3-6]")
    var startingSeeds = 0
    while(startingSeeds < 3 || startingSeeds > 6){
      startingSeeds = getUserInputFromConsole
    }
  }
  println("Goodbye!")

  def createCompVsCompGame(startingSeeds: Int): Unit = {
    val startingPlayerNumber = drawBeginnerNumber()
    val system = ActorSystem("System")
    val board = new Board(startingSeeds, startingPlayerNumber)
    val fstPlayer = system.actorOf(Props(classOf[ComputerPlayer], 0), "fstPlayer")
    val sndPlayer = system.actorOf(Props(classOf[ComputerPlayer], 1), "sndPlayer")
    val server = system.actorOf(Props(classOf[Server], board, fstPlayer, sndPlayer), "server")
    server ! Server.StartGame
  }

  def createHumanVSCompGame(startingSeeds: Int): Unit = {
    val startingPlayerNumber = drawBeginnerNumber()
    val system = ActorSystem("System")
    val board = new Board(startingSeeds, startingPlayerNumber)
    val fstPlayer = system.actorOf(Props(classOf[HumanPlayer], 0), "fstPlayer")
    val sndPlayer = system.actorOf(Props(classOf[ComputerPlayer], 1), "sndPlayer")
    val server = system.actorOf(Props(classOf[Server], board, fstPlayer, sndPlayer), "server")
    server ! Server.StartGame
  }

  def printGameChoiceMenu(): Unit = {
    println("Welcome to Kalah game")
    println("This is (6,s) game variation")
    println("Please choose game mode:")
    println("[1] Computer vs computer")
    println("[2] You vs Computer")
    println("[other] Exit")
  }

  def getUserInputFromConsole: Int = {
    println("Enter correct number:")
    var input = 0
    input = scala.io.StdIn.readInt()
    input
  }

  def drawBeginnerNumber(): Byte = {
    Random.between(0,2).toByte
  }

}
