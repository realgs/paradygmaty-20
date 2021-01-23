import HumanPlayer.ChoosePit
import Server.CheckMove
import akka.actor.Actor

class HumanPlayer(val number: Byte) extends  Actor{
  override def receive: Receive = {
    case ChoosePit(board: Board) =>
      var choice = -1
      println(s"Enter number from ${board.getActivePlayerNumber * 7} to ${board.getActivePlayerNumber * 7 + 5}")
      try {
        choice = scala.io.StdIn.readInt()
      } catch {
        case _ : NumberFormatException => choice = -1
      }
      sender() ! CheckMove(choice)
  }
}

object HumanPlayer{
  case class ChoosePit(boardState: Board)
}
