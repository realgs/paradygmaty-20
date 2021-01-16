package actors

import actors.Server.PLAYER_ONE_INDEX
import akka.actor.{Actor, ActorRef, Props, Timers}
import gameboard.GameBoard

import scala.collection.mutable.ArrayBuffer

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
    }

    case Server.ValidateMove(holeIndex: Int) => {

    }
  }
}

object Server {
  def props: Props = Props[Server]()

  case object StartGame
  case object ConnectToServer
  case object NextMove
  case class ValidateMove(holeIndex: Int)

  private val PLAYER_ONE_INDEX = 0
  private val PLAYER_TWO_INDEX = 1
}
