package scala

import akka.actor.Actor

import scala.ClientRequest._
import scala.ServerRequest._

class Client(player: Player) extends Actor {
  override def receive: Receive = {
    case MOVE =>
      print("Select number (from 1 to 6): ")
      val numberOfHoleToTakeStonesFrom = player.receiveNumber
      print(numberOfHoleToTakeStonesFrom)
      println()
      println("_______________________")
      sender() ! TAKE_FROM_HOLE(numberOfHoleToTakeStonesFrom, player.playerNumber)
  }
}
