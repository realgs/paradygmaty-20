package actors

import Board.Board
import akka.actor._

class Human(val username: String, id: Int, server: ActorRef) extends Player(id)
{
  override protected def makeMove(board: Board): Int =
    {
      println()
      println(board)
      println(s"$username choose a hole 1-6")
      scala.io.StdIn.readInt() - 1
    }
}

object Human
{
  def props(username: String, id: Int, server: ActorRef = null): Props = Props(classOf[Human], username, id, server)
}
