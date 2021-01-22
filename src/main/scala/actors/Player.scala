package actors

import akka.actor._
import Server._
import Board.Board

abstract class Player(val id: Int) extends Actor
{
  override def receive: Receive =
    {
      case RequestMove(board) =>
        sender ! (makeMove(board), id)

      case WrongMove(message) =>
        println(message)
    }

  protected def makeMove(board: Board): Int
}

object Player
{

}

