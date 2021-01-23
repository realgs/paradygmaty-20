import HumanPlayer.ChoosePit
import Server._
import akka.actor.{Actor, ActorRef}

import scala.concurrent.duration.{DurationInt, FiniteDuration}

class Server(val board: Board, val fstPlayer: ActorRef, val sndPlayer: ActorRef) extends Actor{

  private val timeLimit: FiniteDuration = 30 seconds
  var requestStartTime: Long = System.currentTimeMillis()
  var requestEndTime: Long = System.currentTimeMillis()

  override def receive: Receive = {
    case RequestPitNumber =>
      if(board.getActivePlayerNumber == 0) println("*** Player 1 move ***")
      else println("*** Player 2 move ***")
      board.printBoard()

      requestStartTime = System.currentTimeMillis()
      if (board.getActivePlayerNumber == 0) fstPlayer ! ChoosePit(board)
      else sndPlayer ! ChoosePit(board)

    case CheckMove (index: Int) =>
      requestEndTime = System.currentTimeMillis()
      if(requestEndTime - requestStartTime < timeLimit.toMillis){
        if(board.isChosenPitCorrect(index)){
          self ! Move(index)
        }
        else sender() ! ChoosePit(board)
      }
      else self ! Walkover

    case Walkover =>
      if (board.getActivePlayerNumber == 0) {
        println("Player 1 haven't made his move in time")
        println("Player 2 wins!")
      }
      else {
        println("Player 2 haven't made his move in time")
        println("Player 1 wins!")
      }
      context.system.terminate()
      println("Goodbye!")

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
      println("Goodbye!")
  }
}
object Server{
  case object RequestPitNumber
  case object EndGame
  case object Walkover
  case class Move(index: Int)
  case class CheckMove(index : Int)
}