package lab7
import akka.actor.ActorRef

abstract class Message

case class StartGame() extends Message
case class Connect(side: Int) extends Message
case class EndGame() extends Message
case class MakeMove(houseNumb: Int, playerNumb: Int, moveNumb: Int) extends Message
case class TimesUp(playerNumb: Int, moveNumb: Int) extends Message

case class Disconnect() extends Message
case class ComputeMove(houses: Array[Array[Int]], moveNumb: Int) extends Message
case class ComputeMoveAgain(houses: Array[Array[Int]], moveNumb: Int) extends Message

