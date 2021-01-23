import akka.actor.{ActorSystem, Props}

import scala.util.Random

object Game extends App{

  printGameChoiceMenu()
  val usersChoice = getUserInputFromConsole
  if(usersChoice == 1){
    println("You have chosen to watch computer vs computer match")
    println("Please choose starting number of seeds in each pit [3-6]")
    var startingSeeds = 0
    while(startingSeeds < 3 || startingSeeds > 6){
      startingSeeds = getUserInputFromConsole
    }
    createCompVsCompGame(startingSeeds)
  }
  if(usersChoice == 2){
    println("You have chosen to play against vs computer")
    println("You will have 30 seconds to make every one move, otherwise you will lose")
    println("Please choose starting number of seeds in each pit [3-6]")
    var startingSeeds = 0
    while(startingSeeds < 3 || startingSeeds > 6){
      startingSeeds = getUserInputFromConsole
    }
    createHumanVSCompGame(startingSeeds)
  }
  if(usersChoice != 1 && usersChoice != 2) println("Goodbye!")

  def createCompVsCompGame(startingSeeds: Int): Unit = {
    val startingPlayerNumber = drawBeginnerNumber()
    if (startingPlayerNumber == 0) println("Player 1 starts")
    else println("Player 2 starts")
    val system = ActorSystem("System")
    val board = new Board(startingSeeds, startingPlayerNumber)
    val fstPlayer = system.actorOf(Props(classOf[ComputerPlayer], 0.toByte), "fstPlayer")
    val sndPlayer = system.actorOf(Props(classOf[ComputerPlayer], 1.toByte), "sndPlayer")
    val server = system.actorOf(Props(classOf[Server], board, fstPlayer, sndPlayer), "server")
    server ! Server.RequestPitNumber
  }

  def createHumanVSCompGame(startingSeeds: Int): Unit = {
    val startingPlayerNumber = drawBeginnerNumber()
    if (startingPlayerNumber == 0) println("Player 1 starts")
    else println("Player 2 starts")
    val system = ActorSystem("System")
    val board = new Board(startingSeeds, startingPlayerNumber)
    val fstPlayer = system.actorOf(Props(classOf[HumanPlayer], 0.toByte), "fstPlayer")
    val sndPlayer = system.actorOf(Props(classOf[ComputerPlayer], 1.toByte), "sndPlayer")
    val server = system.actorOf(Props(classOf[Server], board, fstPlayer, sndPlayer), "server")
    server ! Server.RequestPitNumber
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
    var input = -1
    try {
      input = scala.io.StdIn.readInt()
    } catch {
      case _ : NumberFormatException => input = -1
    }
    input
  }

  def drawBeginnerNumber(): Byte = {
    Random.between(0,2).toByte
  }

}
