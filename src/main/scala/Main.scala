import akka.actor._
import Board.Board
import actors._

object Main extends App
{
  val system = ActorSystem()
  val board: Board = new Board
  //val player: ActorRef = system.actorOf(Human.props("Krzysiu", 0))
  val bot2: ActorRef = system.actorOf(Bot.props(0))
  val bot: ActorRef = system.actorOf(Bot.props(1))
  val server: ActorRef = system.actorOf(Server.props(board, Array(bot2, bot)))

  server ! Server.PlayGame(0)
}
