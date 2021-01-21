import akka.actor.{Actor, Props}

class ComputerPlayer(val number: Byte) extends Actor{
  override def receive: Receive = ???
}
object ComputerPlayer{
  def props = Props(classOf[ComputerPlayer], number: Byte)
  case class ChoosePit(boardState: Board)
}