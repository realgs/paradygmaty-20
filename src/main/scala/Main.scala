import actors.player.{ComputerPlayer, HumanPlayer}
import actors.server.Server
import akka.actor.{ActorRef, ActorSystem}
import model.Player
import scala.io.StdIn.readInt

object Main extends App {
  val actorSystem = ActorSystem()
  val server: ActorRef = actorSystem.actorOf(Server.props)

  println("Welcome to Kalaha game!")
  println("1. Player vs Computer")
  println("2. Computer vs Computer")
  print("Choose game mode: ")

  readInt() match {
    case 1 => playerVsComputer()
    case 2 => computerVsComputer()
  }

  private def playerVsComputer(): Unit = {
    val playerOne: ActorRef = actorSystem.actorOf(HumanPlayer.props(server, Player.First), "Player1")
    val playerTwo: ActorRef = actorSystem.actorOf(ComputerPlayer.props(server, Player.Second), "Player2")
  }

  private def computerVsComputer(): Unit = {
    val playerOne: ActorRef = actorSystem.actorOf(ComputerPlayer.props(server, Player.First), "Player1")
    val playerTwo: ActorRef = actorSystem.actorOf(ComputerPlayer.props(server, Player.Second), "Player2")
  }
}
