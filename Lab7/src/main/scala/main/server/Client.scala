package main.server

import akka.actor.{Actor, PoisonPill}
import main.game.players.Player

class Client(val player: Player, val log: Boolean) extends Actor {
  override def receive: Receive = {
    case Connect(server) => server ! Connected(player)
    case Disconnect() => self ! PoisonPill

    case DecideMove() => sender() ! MoveDecided(player.decideMove)
    case WrongMove() => if(log) println("Wrong move")
    case MoveTimeout() => if(log) println("Time to move exceeded, move will be chosen randomly")

    case UpdateData(fields: Array[Int], base:Int) => {
      player.fields = fields
      player.base = base
    }
    case PrintText(text) => if(log) println(text)

    case msg => println("Unknown client message: " + msg)
  }
}
