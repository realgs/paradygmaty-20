package actors

import akka.actor._
import Server._
import Board.Board

abstract class Player(val id: Int, val server: ActorRef) extends Actor
{
  override def receive: Receive =
    {
      case Player.RequestMove(board) =>
        println(board)
        server ! Server.MakeMove(makeMove(), id)
    }

  protected def makeMove(): Int
}

object Player
{
  case class RequestMove(board: Board)
}

