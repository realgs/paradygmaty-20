package actors

import akka.actor._
import Board.Board

class Server(private[this] val board: Board, private[this] val players: Array[ActorRef]) extends Actor
{
  override def receive: Receive =
    {
      case Server.MakeMove(move, player_id) =>
        if(validateMove(move, player_id))
        {

        }
        else
        {
          println("Podano złe pole. Zostanie wykonany losowy ruch.")
          val random = scala.util.Random
          changeBoard(random.nextInt(Server.numOfHoles), player_id)
        }

    }

  private def validateMove(move: Int, player_id: Int): Boolean =
    {
      move >= 0 && move <= (Server.numOfHoles - 1) && board.board(player_id)(move) != 0
    }

  private def changeBoard(hole: Int, player_id: Int): Unit =
  {
    val stones = board.board(player_id)(hole)
    
  }
}

object Server
{
  val numOfHoles = 6
  case class MakeMove(move: Int, id: Int)
}
