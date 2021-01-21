// Konrad Karanowski
package ClientServer

import akka.actor._
import Server.{FailureInfo, MakeMove}

import Game.Board

trait Player extends Actor
{
  override def receive: Receive =
  {
    case MakeMove(board) =>
      sender ! makeMove(board)

    case FailureInfo =>
      println(Console.RED + "Move is illegal, the move was chosen for you randomly.")
  }

  protected def makeMove(board: Board): Int
}
