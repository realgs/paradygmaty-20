import HumanPlayer.ChoosePit
import Server.Move
import akka.actor.Actor

import scala.util.Random

class ComputerPlayer(val number: Byte) extends Actor{
  override def receive: Receive = {
    case ChoosePit(board: Board) =>
      var choice = -1
      while(!board.isChosenPitCorrect(choice)){
        choice = Random.between(0, 13)
      }
      println(s"Computer player's choice: $choice")
      sender() ! Move(choice)
  }
}
