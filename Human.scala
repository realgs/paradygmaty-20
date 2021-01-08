package Kalaha

import java.util.Timer

import akka.actor.{Actor, ActorRef, PoisonPill}
import scala.io.StdIn.readLine

class Human(server: ActorRef) extends Actor {

  var playerNumber: Int = _
  var board: Board = _
  var timeOnMove = 30000 //30000 ms
  val timer = new Timer()

  server ! Connect()

  override def receive: Receive = {
    case ReturnPlayerNumber(number) => {
      playerNumber = number
      println("Player (human): " + playerNumber + " get the number")
      server ! Start(self)
    }
    case RequireMove(number, newBoard) => {
      if(playerNumber == number) {
        /*board = newBoard
        var ifValidMove = 0
        var chosenHole: Int = 0
        while(ifValidMove == 0) {
          chosenHole = readLine("Select hole as move: ").toInt
          ifValidMove = board.makeMove(chosenHole, playerNumber)
          if(ifValidMove == 0) println("Invalid move. Try again!")
        }*/
        var chosenHole = 0
        val totalTime = timer({
          chosenHole = chooseMove(newBoard)
        })
        if(totalTime < timeOnMove) {
          println("Player: " + playerNumber + " chose hole number: " + chosenHole)
          server ! MakeMove(chosenHole, playerNumber)
        } else server ! TimesUp(playerNumber)
      }
    }
    /*case InformInvalidMove(newBoard, number) => {
      if(playerNumber == number) {
        println("Player: " + playerNumber + " tries again after invalid move.")
        val chosenHole = ifInvalidMove(newBoard, playerNumber)
        if(chosenHole == -1) server ! TimesUp(playerNumber)
        else server ! MakeMove(chosenHole, playerNumber)
      }
    }*/
    case TurnAgainPlayer(newBoard) => {
      println("Player: " + playerNumber + " moves again.")
      var chosenHole = 0
      val totalTime = timer({
        chosenHole = chooseMove(newBoard)
      })
      if(totalTime < timeOnMove) {
        println("Player: " + playerNumber + " chose hole number: " + chosenHole)
        server ! MakeMove(chosenHole, playerNumber)
      } else server ! TimesUp(playerNumber)
    }
    case Disconnect => {
      self ! PoisonPill
    }
  }

  def timer[A](block: => A): Long = {
    val startTime = System.currentTimeMillis()
    block
    val endTime = System.currentTimeMillis()
    endTime - startTime
  }

  def chooseMove(newBoard: Board): Int = {
    board = newBoard
    var ifValidMove = 0
    var chosenHole: Int = 0
    while(ifValidMove == 0) {
      chosenHole = readLine("Select hole as move: ").toInt
      ifValidMove = board.makeMove(chosenHole, playerNumber)
      if(ifValidMove == 0) println("Invalid move. Try again!")
    }
    chosenHole
  }
}
