import akka.actor.Actor

class ComputerPlayer(val number: Byte) extends Actor{
  override def receive: Receive = ???
}
object ComputerPlayer{
  case class ChoosePit(boardState: Board)
}