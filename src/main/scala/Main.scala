import actors.{ComputerPlayer, Server}
import akka.actor.{ActorRef, ActorSystem}

object Main extends App {
  val actorSystem = ActorSystem()
  val server: ActorRef = actorSystem.actorOf(Server.props)
  //val playerOne: ActorRef = actorSystem.actorOf(ComputerPlayer.props(server), "Player1")
  //val playerTwo: ActorRef = actorSystem.actorOf(ComputerPlayer.props(server), "Player2")

  server ! Server.ValidateMove(3)
  server ! Server.NextMove

  Thread.sleep(5000)

  actorSystem.terminate()
}
