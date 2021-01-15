package ServerPackage

import akka.actor.{Actor, ActorRef}
import javax.swing.JTextPane
import akka.util.Timeout
import akka.pattern.ask
import scala.concurrent.duration.DurationInt
import GameboardPackage.Gameboard
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object Server {
  case class EndGame()                          //Ends the game
  case class DataNotReceived()                  //Ends the game if data not received
  case class UserMoveRequest(board: Gameboard)  //Send packet for user to gain his move
  case class UserMoveReceived(userField: Int)   //Information for server that data was received
  case class ServerAction()                     //Information for server that he should send next packets
}
class Server(private[this] val player1: ActorRef, private[this] val player2: ActorRef,
             private[this] val board: Gameboard, private[this] val gameMessageOutput:JTextPane) extends Actor {

  implicit val timeout: Timeout = {
    Timeout(30.seconds)
  }

  override def receive: Receive = {

    case Server.EndGame() =>
      gameMessageOutput.setText(board.stringResult())
      context.system.terminate()

    case Server.DataNotReceived() =>
      context.system.terminate()

    case Server.UserMoveReceived(userField) =>
      board.playerMove(userField)
      gameMessageOutput.setText(board.toString)
      if (board.endGameCheck()) self ! Server.EndGame()
      else self ! Server.ServerAction()

    case Server.ServerAction() =>
      gameMessageOutput.setText(board.toString)
      if (board.getWhoseRound == 1) {
        val future = player1 ? Server.UserMoveRequest(board)
        future onComplete {
          case Success(userField) =>
            self ! userField
          case Failure(_) =>
            gameMessageOutput.setText("Waited Too long! Game Ends now")
            self ! Server.DataNotReceived()
        }
      } else {
        val future = player2 ? Server.UserMoveRequest(board)
        future onComplete {
          case Success(userField) =>
            self ! userField
          case Failure(_) =>
            gameMessageOutput.setText("Waited Too long! Game Ends now")
            self ! Server.DataNotReceived()
        }
      }
  }
}

