package lab7

import akka.actor._
import lab7.Client._
import lab7.Reader._
import lab7.Server._

class Client(val algorithm: Option[Algorithm] = None, val reader: ActorRef, val name: String) extends Actor {
  private var server: Option[ActorRef] = None
  private var color = ""

  def receive = {
    case Init(serv: ActorRef, playerColor: Player ) => {
      server = Some(serv)
      if (playerColor == Yellow) color = Console.YELLOW
      else color = Console.BLUE
    }
    case MoveRequest(position, seconds) => {
      println(position)
      algorithm match {
        case None => reader ! AskInt(color + s"your turn (you've got $seconds seconds)" + Console.RESET, self)
        case Some(alg) => server.get ! Move(alg.move(position))
      }
    }
    case ResponseInt(answer) => {
      server.get ! Move(answer)
    }
    case PositionUpdated(position) => {
      println(position)
    }
    case End(reason) => {
      ???
      reader ! Withdraw("timeout")
    }
    case InvalidMove(seconds) => {
      reader ! AskInt(color + s"Invalid Move. \r\n your turn (you've got $seconds seconds)" + Console.RESET, self)
    }
    case Timeout() => {
      reader ! Withdraw("timeout! random move was made")
    }
  }
}

object Client {
  def props(reader: ActorRef, name: String, algorithm: Option[Algorithm] = None): Props =
    Props(new Client(algorithm, reader, name))

  final case class Init(serv: ActorRef, playerColor: Player)
  final case class MoveRequest(position: Position, seconds: Int)
  final case class PositionUpdated(position: Position)
  final case class End(reason: Reason)
  final case class InvalidMove(seconds: Int)
  final case class Timeout()
}
