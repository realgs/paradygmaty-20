package scala

import akka.actor.Actor

import ClientRequest._
import ServerRequest._

class Client(player: Player) extends Actor{
  override def receive: Receive = {
    case MOVE(availableHoles: List[Int]) =>
      print("Select number (from 1 to 6): ")
      val numberOfHoleToTakeStonesFrom = player.receiveNumber
      print(numberOfHoleToTakeStonesFrom)
      println()
      println("_______________________")
      sender() ! TAKE_FROM_HOLE(numberOfHoleToTakeStonesFrom, player.playerNumber)
  }
}
