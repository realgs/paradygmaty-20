package Player

import Server._
import akka.actor.Actor

class HumanPlayer(private val ifFirstPlayer: Boolean) extends Actor {

   override def receive: Receive = {
      case Server.ChooseField(board) =>
        var chosenField = -1

        while(!board.ifChosenFieldIsCorrect(chosenField)) {
          print("Give field number: ")
          try {
            chosenField = scala.io.StdIn.readInt()
          } catch {
            case _ : NumberFormatException => chosenField = -1
          }
        }

        sender ! Server.ConfirmMyMove(chosenField)
  }

}
