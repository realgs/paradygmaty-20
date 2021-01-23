import akka.actor.Actor

import scala.io.StdIn

class HumanPlayer extends Actor {

  override def receive: Receive = {

    case Server.MakeMove(board: Board) =>
      sender ! Server.Move(makeMove(board))
  }

  private def makeMove(board: Board): Int = {

    var fieldNumber: Int = 0

    while(!board.ifCorrectFieldNumber(fieldNumber)) {

      print("Chose field (1-6): ")

      try {

        fieldNumber = StdIn.readInt()
        if(!board.ifCorrectFieldNumber(fieldNumber))
          println("\nWrong field number! Try again.")

      } catch {

        case _: NumberFormatException => println("\nWrong input! Try again.")
        case _: Exception => ()
      }
    }

    fieldNumber
  }
}