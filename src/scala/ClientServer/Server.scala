// Konrad Karanowski
package ClientServer

import akka.actor._
import akka.util.Timeout
import akka.pattern.ask
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import scala.util.Random

import ClientServer.Server._
import Game.Board

class Server(private[this] val bottomPlayer: ActorRef, private[this] val topPlayer: ActorRef) extends Actor
{
  implicit val timeout: Timeout = 30.seconds
  private[this] val board: Board = Board(6)
  override def receive: Receive =
  {
    case EndGame =>
      displayEndGameResults()
      context.system.terminate()

    case EndGameOutOfTime(bottomPlayerMove) =>
      giveUp(bottomPlayerMove)
      context.system.terminate()

    case Play(bottomPlayerMove) =>
      displayBoardAndPlayer(bottomPlayerMove)
      play(bottomPlayerMove)
  }

  private def displayEndGameResults(): Unit =
  {
    println(Console.YELLOW + "Game is over!")
    board.printBoard()
    val bottomPlayerScore = board.bottomPlayerMancala
    val topPlayerScore = board.topPlayerMancala
    if (bottomPlayerScore > topPlayerScore)
      println("Bottom player is the winner!")
    else if (bottomPlayerScore == topPlayerScore)
      println("Draw!")
    else
      println(Console.BLUE + "Top player is the winner!")
    println(s"Result: $bottomPlayerScore : $topPlayerScore")
  }

  private def giveUp(bottomPlayerMove: Boolean): Unit =
  {
    println(Console.RED + "Out of time! The game is over.")
    if(bottomPlayerMove) println(Console.BLUE + "Top player is a winner!")
    else println(Console.GREEN + "Bottom player is a winner!")
  }

  private def displayBoardAndPlayer(bottomPlayerMove: Boolean): Unit =
  {
    println(s"${if(bottomPlayerMove) Console.GREEN+ "Bottom player" else Console.BLUE + "Top player"} move")
    board.printBoard()
  }

  private def play(bottomPlayerMove: Boolean): Unit =
  {
    if (bottomPlayerMove)
    {
      val moveRequest = bottomPlayer ? MakeMove(board)
      moveRequest.onComplete {
        case Success(move: Int) =>
          validateMove(true, move)
        case Failure(_) =>
          self ! EndGameOutOfTime(true)
      }
    }
    else
    {
      val moveRequest = topPlayer ? MakeMove(board)
      moveRequest.onComplete {
        case Success(move: Int) =>
          validateMove(false, move)
        case Failure(_) =>
          self ! EndGameOutOfTime(false)
      }
    }
  }

  private def validateMove(bottomPlayerMove: Boolean, move: Int): Unit =
  {
    if (bottomPlayerMove)
    {
      if(board.isLegalBottom(move))
      {
        val hasNext = board.bottomPlayerMove(move)
        if (board.isFinished) self ! EndGame
        else self ! Play(hasNext)
      }
      else
      {
        val hasNext = playRandomMove(true)
        bottomPlayer ! FailureInfo
        if (board.isFinished) self ! EndGame
        else self ! Play(hasNext)
      }
    }
    else
    {
      if(board.isLegalTop(move))
      {
        val hasNext = board.topPlayerMove(move)
        if (board.isFinished) self ! EndGame
        else self ! Play(!hasNext)
      }
      else
      {
        val hasNext = playRandomMove(false)
        topPlayer ! FailureInfo
        if (board.isFinished) self ! EndGame
        else self ! Play(!hasNext)
      }
    }
  }

  private def randomMove(bottomPlayerMove: Boolean): Int =
  {
    @scala.annotation.tailrec
    def randomizeMove(): Int =
    {
      val move = Random.between(0, 6)
      if (bottomPlayerMove && board.isLegalBottom(move)) move
      else if (!bottomPlayerMove && board.isLegalTop(move)) move
      else randomizeMove()
    }
    randomizeMove()
  }

  private def playRandomMove(bottomPlayerMove: Boolean): Boolean =
  {
    if (bottomPlayerMove) board.bottomPlayerMove(randomMove(true))
    else board.topPlayerMove(randomMove(false))
  }
}


object Server
{
  // server info
  case object EndGame
  case class EndGameOutOfTime(bottomPlayerMove: Boolean)
  case class Play(bottomPlayerMove: Boolean)

  // communication with player
  case class MakeMove(board: Board)
  case object FailureInfo
}
