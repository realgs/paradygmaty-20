import java.util.concurrent.TimeoutException

import akka.actor.{Actor, ActorRef}
import akka.util.Timeout

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

//TODO Handle hanging scala.io.StdIn.readLine() call on timeout
class Client(server: ActorRef, player: Player) extends Actor {
  implicit val timeout: Timeout = akka.util.Timeout(5.seconds)

  override def receive: Receive = {
    case Start => server ! AskForApproval(player.number)
    case DemandAccepted => server ! GetGameStatus
    case DemandRejected => Thread.sleep(1000); server ! AskForApproval(player.number)
    case InvalidMove => println("Invalid move!"); server ! GetGameStatus
    case ObtainGameStatus(gameStatus: Game) => makeAMove(gameStatus)
    case ValidMove => server ! AskForApproval(player.number)
  }

  private def makeAMove(gameStatus: Game): Unit = {
    try {
      val currentTime = System.currentTimeMillis()
      val chosenHole = chooseAHole(gameStatus)
      println(System.currentTimeMillis() - currentTime)
      server ! MakeAMove(chosenHole)
    }
    catch {
      case _: TimeoutException => println();println("Request timed out!"); server ! MakeRandomMove
      case _: NumberFormatException => println();println("Wrong input given!"); server ! MakeRandomMove
    }
  }

  private def chooseAHole(gameStatus: Game): Int = {
    val smallestNumber = 7 * (player.number - 1)
    val biggestNumber = smallestNumber + 5
    print(s"${player.name} - choose a hole($smallestNumber - $biggestNumber): ")
    val futureInput = Future {
      player.play(gameStatus)
    }
    Await.result(futureInput, 30.seconds)
  }
}

//Actions handled by Client
case object Start

case object DemandAccepted

case object DemandRejected

case object InvalidMove

case object ValidMove

case class ObtainGameStatus(gameStatus: Game)