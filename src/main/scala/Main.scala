import actors.player.ComputerPlayer
import actors.server.{Server, ServerActions}
import akka.actor.{ActorRef, ActorSystem}
import model.Player

object Main extends App {
  val actorSystem = ActorSystem()

  val server: ActorRef = actorSystem.actorOf(Server.props)
  val playerOne: ActorRef = actorSystem.actorOf(ComputerPlayer.props(server, Player.First), "Player1")
  val playerTwo: ActorRef = actorSystem.actorOf(ComputerPlayer.props(server, Player.Second), "Player2")
}
