import Server.{AskForMove, EndGame, MakeMove, Move}
import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

class Server(val player1: ActorRef, val player2: ActorRef, val board: Board) extends Actor {

  private implicit val timeout: Timeout = Timeout(30.seconds)

  override def receive: Receive = {

    case AskForMove =>
      printf("%n%n# PLAYER %d. MOVE", if(board.isPlayer1Move) 1 else 2)
      if(board.isPlayer1Move) waitForMove(player1)
      else waitForMove(player2)

    case Move(fieldNumber) =>
      printf("%n# PLAYER %d. HAS CHOSEN %d%n", if(board.isPlayer1Move) 1 else 2, fieldNumber)
      board.makeMove(fieldNumber)
      board.print()
      if(board.isGameOver) self ! EndGame
      else self ! AskForMove

    case EndGame =>
      println("\n\n# THE GAME IS OVER")
      board.printResult()
      context.system.terminate()
  }

  private def waitForMove(player: ActorRef): Unit = {

    val move = player ? MakeMove(board)

    move.onComplete {

      case Success(value) => self ! value
      case Failure(_) =>
        printf("%n%n# TIME IS UP! PLAYER %d. WON!%n%n# END OF THE GAME!%n", if(board.isPlayer1Move) 2 else 1)
        context.system.terminate()
//        System.exit(0)
    }
  }
}

object Server {

  case object AskForMove
  case class MakeMove(board: Board)
  case class Move(fieldNumber: Int)
  case object EndGame
}