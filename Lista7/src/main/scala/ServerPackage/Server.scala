package ServerPackage

import akka.actor.{Actor, ActorRef}
import javax.swing.JTextArea
import akka.util.Timeout
import akka.pattern.ask
import scala.concurrent.duration.DurationInt
import GameboardPackage.Gameboard
import scala.util.{Failure, Success}

object Server {
  case class EndGame();
  case class DataNotReceived()
  case class UserMoveRequest(board: Gameboard)
  case class UserMoveReceived(userField: Int)
  case class ServerAction(gameMessageOutput: JTextArea)
}
class Server(val player1: ActorRef, val player2: ActorRef, val board: Gameboard) extends Actor {
  implicit val timeout: Timeout = Timeout(30 seconds)
  override def receive: Receive = {
    case Server.ServerAction(gameMessageOutput) =>
      gameMessageOutput.setText(board.toString)
      if (board.getWhoseRound() == 1) {
        val future = player1 ? Server.UserMoveRequest(board)
        future onComplete {
          case Success(value) =>
            self ! value // this ! Server.ConfirmMyMove(chosenField)
          case Failure(_) =>
            self ! Server.DataNotReceived()
        }
      }
}
