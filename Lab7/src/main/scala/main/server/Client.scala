package main.server

import akka.actor.{Actor, PoisonPill}
import main.game.players.Player

class Client(val player: Player, val log: Boolean) extends Actor {
  override def receive: Receive = {
    case Connect(server) => server ! Connected(player)
    case Disconnect() => self ! PoisonPill

    case DecideMove => sender() ! player.decideMove
    case PrintText(text) => if(log) println(text)

    case _ => println("Unknown client message")
  }
}
