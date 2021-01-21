import akka.actor.Actor

class HumanPlayer(val number: Byte) extends  Actor{
  override def receive: Receive = ???
}

object HumanPlayer{
  case class ChoosePit(boardState: Board)
}
