import akka.actor.{Actor, ActorRef, Props}

class Server(val board: Board, val fstPlayer: ActorRef, val sndPlayer: ActorRef) extends Actor{
  override def receive: Receive = ???
}
object Server{
  def prop = Props(classOf[Server], board: Board, fstPlayer: ActorRef, sndPlayer: ActorRef)
  case object StartGame
  case object EndGame
  case class Walkover(playerNumber: Byte)
  case class Move(index: Int)
}