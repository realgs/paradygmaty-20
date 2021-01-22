import HumanPlayer.ChoosePit
import Server.{CheckMove, Move}
import akka.actor.Actor

class HumanPlayer(val number: Byte) extends  Actor{
  override def receive: Receive = {
    case ChoosePit(board: Board) =>
      var choice = -1
//      while(!board.isChosenPitCorrect(choice)){
//        println(s"Enter number from ${board.getActivePlayerNumber * 7} to ${board.getActivePlayerNumber * 7 + 5}")
//        choice = scala.io.StdIn.readInt()
//      }
      println(s"Enter number from ${board.getActivePlayerNumber * 7} to ${board.getActivePlayerNumber * 7 + 5}")
      choice = scala.io.StdIn.readInt()
      sender() ! CheckMove(choice)
  }
}

object HumanPlayer{
  case class ChoosePit(boardState: Board)
}
