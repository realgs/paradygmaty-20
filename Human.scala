package Kalaha

import akka.actor.{Actor, ActorRef, PoisonPill}
import scala.io.StdIn.readLine

class Human(server: ActorRef) extends Actor {

  private var playerNumber: Int = _
  private val timeOnMove = 30000 //30s
  private val miliSec = 1000
  private var totalTimeFirstMove:Long = _
  private var totalTimeNextMove:Long = _

  server ! Connect()

  override def receive: Receive = {
    case ReturnPlayerNumber(number) => {
      playerNumber = number
      println("Player (human): " + playerNumber + " get the number")
      server ! Start(self)
    }
    case RequireMove(number, newBoard) => {
      if(playerNumber == number) {
        makeMove()
      }
    }
    case TurnAgainPlayer(newBoard) => {
      makeMove()
    }
    case InformInvalidMove(newBoard, playerNumber) => {
      var chosenHole = 0
      println("You have more " + (timeOnMove - totalTimeFirstMove)/miliSec + " seconds to move again.")
      totalTimeNextMove = timer({
        chosenHole = readLine("Select hole as move: ").toInt
      })
      val totalTime = totalTimeNextMove + totalTimeFirstMove
      totalTimeFirstMove = totalTime
      println("You moved after: " + totalTime/miliSec + " seconds")
      if(totalTime > timeOnMove) {
        server ! TimesUp(playerNumber)
      } else {
        server ! MakeMove(chosenHole, playerNumber)
      }
    }
    case Disconnect => {
      println("Disconnect - human: player " + playerNumber)
      self ! PoisonPill
    }
  }

  def timer[A](block: => A): Long = {
    val startTime = System.currentTimeMillis()
    block
    val endTime = System.currentTimeMillis()
    endTime - startTime
  }

  def makeMove(): Unit = {
    var chosenHole = 0
    totalTimeFirstMove = timer({
      chosenHole = readLine("Select hole as move: ").toInt
    })
    println("You needed " + totalTimeFirstMove/miliSec + " seconds to move.")
    if(totalTimeFirstMove > timeOnMove) {
      server ! TimesUp(playerNumber)
    } else {
      server ! MakeMove(chosenHole, playerNumber)
    }
  }
}
