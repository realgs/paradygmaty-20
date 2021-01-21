import akka.actor.{Actor, Props}

class HumanPlayer(val number: Byte) extends  Actor{
  override def receive: Receive = ???
}

object HumanPlayer{
  def props = Props(classOf[HumanPlayer], number: Byte)
  case class ChoosePit(boardState: Board)
}
