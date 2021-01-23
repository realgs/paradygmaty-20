import akka.actor.Actor

import scala.io.StdIn

class ComputerPlayer extends Actor {

  override def receive: Receive = {
    case Server.MakeMove(board: Board) =>
      sender ! Server.Move(makeMove(board))
  }

  def makeMove(board: Board): Int = StdIn.readInt()
}
