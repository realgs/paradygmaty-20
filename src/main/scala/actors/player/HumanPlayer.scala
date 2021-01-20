package actors.player

import actors.player.HumanPlayer.VALIDATE_MOVE_TIMEOUT
import actors.server.ServerActions
import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import model.Player

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.io.StdIn.readInt

class HumanPlayer(server: ActorRef, player: Player.Value) extends Actor {

  private implicit val validateMoveTimeout: Timeout = Timeout(VALIDATE_MOVE_TIMEOUT)

  server ! ServerActions.ConnectToServer(player.id)

  override def receive: Receive = {
    case PlayerActions.MakeMove(_) =>
      print("Choose index: ")

      var holeIndex = readInt()

      while(!validateMove(holeIndex)) {
        println("This move is invalid!")
        print("Choose another index: ")

        holeIndex = readInt()
      }

      sender() ! holeIndex
    }

  private def validateMove(holeIndex: Int): Boolean = {
    val future = server ? ServerActions.ValidateMove(holeIndex)
    Await.result(future, VALIDATE_MOVE_TIMEOUT).asInstanceOf[Boolean]
  }
}

object HumanPlayer {
  def props(server: ActorRef, player: Player.Value): Props = Props(classOf[HumanPlayer], server, player)

  private val VALIDATE_MOVE_TIMEOUT = 2.seconds
}
