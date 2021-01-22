package actors

import akka.actor._
import Server._
import Board.Board

abstract class Player(val id: Int) extends Actor
{
  override def receive: Receive =
    {
      case RequestMove(board) =>
        println()
        println(board)
        sender ! (makeMove(), id)

      case WrongMove(message) =>
        println(message)
    }

  protected def makeMove(): Int
}

object Player
{

}

