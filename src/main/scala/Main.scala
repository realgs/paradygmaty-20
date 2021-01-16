import actors.{ComputerPlayer, Server}
import akka.actor.{ActorRef, ActorSystem}

object Main extends App {
  val actorSystem = ActorSystem()
  val server: ActorRef = actorSystem.actorOf(Server.props)
  val player1: ActorRef = actorSystem.actorOf(ComputerPlayer.props(server), "Player1")
  val player2: ActorRef = actorSystem.actorOf(ComputerPlayer.props(server), "Player2")

  actorSystem.terminate()
}
