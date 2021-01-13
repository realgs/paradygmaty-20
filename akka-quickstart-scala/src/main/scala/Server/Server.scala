package Server
import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

object Server {
  case object MakeMove //send from server to server to launch making move procedure
  case class ChooseField(board: Board) //send from server to player to get the field he chose
  case class ConfirmMyMove(chosenField: Int) //send from server to server to make move procedure
  case object EndGame // end of the game
  case object AbortGame //one of the player didn't make a move - end the game
}

class Server(val player1: ActorRef, val player2: ActorRef, val board: Board) extends Actor {
  implicit val timeout: Timeout = Timeout(30 seconds)

  override def receive: Receive = {
    case Server.MakeMove =>
      board.printBoard()
      if(board.ifFirstPlayerRound) {
        val future = player1 ? Server.ChooseField(board)
        future onComplete {
          case Success(value) =>
            self ! value // this ! Server.ConfirmMyMove(chosenField)
          case Failure(_) =>
            println("")
            println("Waited too long")
            println("Player 1 lost this game!")
            self ! Server.AbortGame
        }
      } else {
        val future = player2 ? Server.ChooseField(board)
        future onComplete {
          case Success(value) =>
            self ! value // this ! Server.ConfirmMyMove(chosenField)
          case Failure(_) =>
            println("")
            println("Waited too long")
            println("Player 2 lost this game!")
            self ! Server.AbortGame
        }
      }

    case Server.ConfirmMyMove(chosenField) =>
      board.makeMove(chosenField)
      board.takeEnemyStonesIfPossible()
      board.nextPlayer()
      if(board.isGameFinished) //if next player cant make its move
        self ! Server.EndGame
      else
        self ! Server.MakeMove

    case Server.EndGame =>
      println("")
      println("END OF THE GAME !")
      board.showResults()
      context.system.terminate()

    case Server.AbortGame =>
      context.system.terminate()

  }
}
