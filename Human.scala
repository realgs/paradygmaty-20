package Kalaha

import java.util.Timer
import akka.actor.{Actor, ActorRef, PoisonPill}
import scala.io.StdIn.readLine

class Human(server: ActorRef) extends Actor {

  var playerNumber: Int = _
  var boardHuman: Board = _
  var timeOnMove = 30000 //30s
  val timer = new Timer()
  var totalTimeFirstMove:Long = _
  var totalTimeNextMove:Long = _

  server ! Connect()

  override def receive: Receive = {
    case ReturnPlayerNumber(number) => {
      playerNumber = number
      println("Player (human): " + playerNumber + " get the number")
      server ! Start(self)
    }
    case RequireMove(number, newBoard) => {
      if(playerNumber == number) {
        var chosenHole = 0
        totalTimeFirstMove = timer({
          chosenHole = readLine("Select hole as move: ").toInt
        })
        println(totalTimeFirstMove)
        if(totalTimeFirstMove > timeOnMove) {
          server ! TimesUp(playerNumber)
        } else {
          server ! MakeMove(chosenHole, playerNumber)
        }
      }
    }
    case TurnAgainPlayer(newBoard) => {
      makeMove()
    }
    case InformInvalidMove(newBoard, playerNumber) => {
      var chosenHole = 0
      totalTimeNextMove = timer({
        chosenHole = readLine("Select hole as move: ").toInt
      })
      println("Total time next move: " + totalTimeNextMove)
      val totalTime = totalTimeNextMove + totalTimeFirstMove
      totalTimeNextMove = totalTime
      println("Total time: " + totalTime)
      if(totalTime > timeOnMove) {
        server ! TimesUp(playerNumber)
      } else {
        server ! MakeMove(chosenHole, playerNumber)
      }
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

  def makeMove(): Unit = {
    var chosenHole = 0
    totalTimeFirstMove = timer({
      chosenHole = readLine("Select hole as move: ").toInt
    })
    println(totalTimeFirstMove)
    if(totalTimeFirstMove > timeOnMove) {
      server ! TimesUp(playerNumber)
    } else {
      server ! MakeMove(chosenHole, playerNumber)
    }
  }
}
