package Player

import Server.Server
import akka.actor.Actor

class RandomPlayer(private val ifFirstPlayer: Boolean) extends Actor {
  val random = new scala.util.Random

  override def receive: Receive = {
    case Server.ChooseField(board) =>
      var chosenField = -1
      var lowerHole = 0
      var upperHole = 5
      if(!ifFirstPlayer) {
        lowerHole += 7
        upperHole += 7
      }

      while(!board.ifChosenFieldIsCorrect(chosenField)) {
        chosenField = lowerHole + random.nextInt(upperHole - lowerHole + 1)
      }

      print("Computer choose: " + chosenField)
      sender ! Server.ConfirmMyMove(chosenField)
  }

}
