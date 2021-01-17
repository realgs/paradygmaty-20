package actors

import actors.Server.{PLAYER_ONE_INDEX, TIMER_KEY, TURN_TIME}
import akka.actor.{Actor, ActorRef, Props, Timers}
import gameboard.GameBoard

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.DurationInt

class Server extends Actor with Timers {

  private val playersList = ArrayBuffer[ActorRef]()
  private var gameBoard: GameBoard = _

  override def receive: PartialFunction[Any, Unit] = {
    case Server.ConnectToServer => {
      playersList += sender
      println("Some player has connected to the server")
    }

    case Server.StartGame => {
      gameBoard = new GameBoard()
      playersList(PLAYER_ONE_INDEX) ! PlayerActions.MakeMove(gameBoard.clone())

      startTimer()
    }

    case Server.ValidateMove(holeIndex: Int) => {
      startTimer()
      println("test")

      Thread.sleep(1100)

      stopTimer()
    }

    case Server.NextMove => {
    }

    case Server.Timeout => {
      println("TIMEOUT")
    }
  }

  private def startTimer(): Unit = {
    timers.startSingleTimer(TIMER_KEY, Server.Timeout, TURN_TIME)
  }

  private def stopTimer(): Unit = {
    timers.cancel(TIMER_KEY)
  }
}

object Server {
  def props: Props = Props[Server]()

  case object StartGame
  case object ConnectToServer
  case object NextMove
  case object Timeout
  case class ValidateMove(holeIndex: Int)

  private val TURN_TIME = 1000.millis

  private val PLAYER_ONE_INDEX = 0
  private val PLAYER_TWO_INDEX = 1

  private val TIMER_KEY = "SERVER_TIMER"
}
