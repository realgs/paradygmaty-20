package pl.pawelklecha.kalaha
package server

import events.{PlayerActionEvent, ServerActionEvent}
import players.Player
import server.Server.TIMEOUT

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

class Server(private val player1: ActorRef, private val player2: ActorRef, private val gameboard: Gameboard) extends Actor {
  implicit val timeout: Timeout = Timeout(TIMEOUT)

  override def receive: Receive = {
    case ServerActionEvent.NextMove =>
      gameboard.printGameBoard()
      val future = gameboard.getCurrentPlayer match {
        case Player.PLAYER_1 => player1 ? ServerActionEvent.MakeMove(gameboard)
        case Player.PLAYER_2 => player2 ? ServerActionEvent.MakeMove(gameboard)
      }
      future onComplete {
        case Success(value) => self ! value
        case Failure(_) =>
          println("Waited too long")
          println(s"${gameboard.getCurrentPlayer} lost this game!")
          self ! ServerActionEvent.AbortGame
      }

    case PlayerActionEvent.ConfirmMove(chosenField) =>
      gameboard.makeMove(chosenField)
      if (gameboard.checkEndGameStatus()) {
        self ! ServerActionEvent.EndGame
      } else {
        self ! ServerActionEvent.NextMove
      }

    case ServerActionEvent.EndGame =>
      gameboard.printResult()
      context.system.terminate()

    case ServerActionEvent.AbortGame => context.system.terminate()
  }
}

object Server {
  private val TIMEOUT = 30 seconds
}
