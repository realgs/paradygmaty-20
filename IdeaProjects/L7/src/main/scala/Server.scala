import akka.actor.Actor

case class Move(pit: Int)
case object Stop
case object Start

class Server extends Actor{
  override def receive: Receive = {
    case Start => sender() ! MakeAMove
    case Move(pit) => {

    }

  }
}
