import akka.actor.{Actor, ActorRef}

class Server(val board: Board, val fstPlayer: ActorRef, val sndPlayer: ActorRef) extends Actor{
  override def receive: Receive = ???
}
object Server{
  case object StartGame
  case object EndGame
  case class Walkover(playerNumber: Byte)
  case class Move(index: Int)
}