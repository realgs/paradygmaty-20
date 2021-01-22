import HumanPlayer.ChoosePit
import Server.{EndGame, Move, RequestPitNumber, Walkover}
import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

class Server(val board: Board, val fstPlayer: ActorRef, val sndPlayer: ActorRef) extends Actor{
  implicit val timeout: Timeout = Timeout(30 seconds)
  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  override def receive: Receive = {
    case RequestPitNumber =>
      board.printBoard()
      if (board.getActivePlayerNumber == 0) {
        val futureResult = fstPlayer ? ChoosePit(board)
        futureResult onComplete {
          case Success(result) => self ! result
          case Failure(_) => self ! Walkover
        }
      }
      else {
        val futureResult = sndPlayer ? ChoosePit(board)
        futureResult onComplete {
          case Success(result) => self ! result
          case Failure(_) => self ! Walkover
        }
      }

    case Walkover =>
      if (board.getActivePlayerNumber == 0) {
        println("Player 1 haven't made his move in time")
        println("Player 2 wins!")
      }
      else {
        println("Player 1 haven't made his move in time")
        println("Player 2 wins!")
      }
      context.system.terminate()

    case Move(pitNumber: Int) =>
      board.moveSeedsFrom(pitNumber)
      board.determineNextPlayerNumber()
      if(board.isNextMovePossible) self ! RequestPitNumber
      else self ! EndGame

    case EndGame =>
      println("Final board setup before summary:")
      board.printBoard()
      board.endGameCollectSeeds()
      board.printEndGameResult()
      context.system.terminate()
  }
}
object Server{
  case object RequestPitNumber
  case object EndGame
  case object Walkover
  case class Move(index: Int)
}