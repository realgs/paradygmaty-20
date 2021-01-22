package actors

import akka.actor._
import akka.pattern.ask
import Board.Board
import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}

class Server(private[this] val board: Board, private[this] val players: Array[ActorRef]) extends Actor
{
  implicit val timeout: Timeout = 30.seconds

  override def receive: Receive =
    {
      case Server.PlayGame(player_id) =>
        playGame(player_id)

      case Server.Terminate =>
        context.system.terminate()

      case Server.TerminateOutOfTime =>
        println("Game terminated due to timeout of player's move")
        context.system.terminate()
    }

  private def playGame(player_id: Int): Unit =
  {
    val request = players(player_id) ? Server.RequestMove(board)

    request.onComplete {
      case Success((move: Int, id: Int)) =>
        makeMove(move, id)

      case Failure(_) =>
        self ! Server.TerminateOutOfTime
    }
  }

  private def makeMove(move: Int, player_id: Int): Unit =
  {
    var repeat = false
    if(board.validateMove(move, player_id))
      repeat = board.makeMovesOnBoard(move, player_id)
    else
    {
      players(player_id) ! Server.WrongMove("Podano złe pole. Zostanie wykonany losowy ruch.")
      val random = scala.util.Random
      val validMoves = board.findValidMoves(player_id)
      val random_move = validMoves(random.nextInt(validMoves.length))
      repeat = board.makeMovesOnBoard(random_move, player_id)
    }

    val isEnd = board.checkGameEnd()
    if (isEnd == -1)
    {
      if (repeat)
        self ! Server.PlayGame(player_id)
      else
        self ! Server.PlayGame((player_id + 1) % 2)
    }
    else
    {
      board.gatherPlayersStones(isEnd)
      val winner_id = board.checkWhoWon()
      if (winner_id == -1)
        println("Remis!")
      else
        println(s"Wygrał gracz: ${winner_id + 1}. Punkty gracza: ${board.getPoints(winner_id)} | Punkty przeciwnika: ${board.getPoints((winner_id + 1) % 2)}")

      self ! Server.Terminate
    }
  }
}

object Server
{
  val numOfHoles = 6
  val numOfPlayers = 2
  case class PlayGame(id: Int)
  case class RequestMove(board: Board)
  case class WrongMove(message: String)
  case object Terminate
  case object TerminateOutOfTime

  def props(board: Board, players: Array[ActorRef]): Props = Props(classOf[Server], board, players)
}
