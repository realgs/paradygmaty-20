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
    if(validateMove(move, player_id))
      repeat = makeMovesOnBoard(move, player_id)
    else
    {
      players(player_id) ! Server.WrongMove("Podano złe pole. Zostanie wykonany losowy ruch.")
      val random = scala.util.Random
      val validMoves = findValidMoves(player_id)
      val random_move = validMoves(random.nextInt(validMoves.length))
      repeat = makeMovesOnBoard(random_move, player_id)
    }

    val isEnd = checkGameEnd(board)
    if (isEnd == -1)
    {
      if (repeat)
        self ! Server.PlayGame(player_id)
      else
        self ! Server.PlayGame((player_id + 1) % 2)
    }
    else
    {
      gatherPlayersStones(isEnd)
      val winner_id = checkWhoWon()
      if (winner_id == -1)
        println("Remis!")
      else
        println(s"Wygrał gracz: ${winner_id + 1}. Punkty gracza: ${board.board(winner_id)(6)} | Punkty przeciwnika: ${board.board((winner_id + 1) % 2)(6)}")

      self ! Server.Terminate
    }
  }

  private def validateMove(move: Int, player_id: Int): Boolean =
    {
      move >= 0 && move <= (Server.numOfHoles - 1) && board.board(player_id)(move) != 0
    }

  private def makeMovesOnBoard(hole: Int, player_id: Int): Boolean =
  {
    """
      |Method makes moves with stones for a player. Returns true if last stone landed in player's base. False otherwise.
      |""".stripMargin

    val stones = board.board(player_id)(hole)
    board.board(player_id)(hole) = 0

    var hole_id = hole
    var row = player_id

    for (_ <- 0 until stones)
      {
        hole_id += 1
        if (hole_id > 6)
          {
            hole_id = 0
            row = (row + 1) % 2
          }
        board.board(row)(hole_id) += 1
      }

    if (hole_id == 6 && player_id == row)
      true
    else if (player_id == row && board.board(row)(hole_id) == 1)
      {
        board.board(player_id)(6) += board.board((player_id + 1) % 2)(5 - hole_id)
        board.board((player_id + 1) % 2)(5 - hole_id) = 0
        false
      }
    else false
  }

  private def checkGameEnd(board: Board): Int =
    {
      """
        |Checks if either player has no stones in any of his holes. Returns -1 if both players have at least one non-empty hole or
        |id of player that still has stones in any hole.
        |""".stripMargin


      for(i <- 0 until Server.numOfPlayers)
        {
          var hasStones = false
          for (j <- 0 until Server.numOfHoles)
          {
            if (board.board(i)(j) > 0)
              hasStones = true
          }
          if(!hasStones)
            return (i + 1) % 2
        }

      -1
    }

  private def gatherPlayersStones(player_id: Int): Unit =
  {
    for (i <- 0 until Server.numOfHoles)
      {
        board.board(player_id)(6) += board.board(player_id)(i)
        board.board(player_id)(i) = 0
      }
  }

  private def checkWhoWon(): Int =
  {
    if (board.board(0)(6) == board.board(1)(6))
      -1
    else if(board.board(0)(6) > board.board(1)(6))
      0
    else
      1
  }

  def findValidMoves(player_id: Int): Seq[Int] =
    {
      var seq = Seq[Int]()
      for (i <- 0 to 5)
        if(board.board(player_id)(i) > 0)
          seq = seq.appended(i)
      seq
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
