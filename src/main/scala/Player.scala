import akka.actor.{Actor, Props}

abstract class Player(val id: Int, val kalah: Int, val pits: List[Int]) extends Actor {
  var m_board: Map[Int, Int] = _

  final override def receive: Receive = {
    case Player.MoveRequest(board) =>
      m_board = board
      sender() ! GameServer.MoveReply(choosePit())
    case _ => println("Unknown request to client!")
  }


  def choosePit(): Int
}

object Player {
  case class MoveRequest(board: Map[Int, Int])
  def props(id: Int, kalah: Int, pits: List[Int], is_human: Int): Props = {
    if(is_human == 1)
      Props(new HumanPlayer(id, kalah, pits))
    else
      Props(new ComputerPlayer(id, kalah, pits))
  }
}
