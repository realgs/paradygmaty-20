package main.server

import akka.actor.ActorRef
import main.game.players.Player

abstract class Request

// Server requests
case class Connected(player: Player) extends Request
case class Disconnected() extends Request

case class StartGame() extends Request

// Client requests
case class Connect(server: ActorRef) extends Request
case class Disconnect() extends Request

case class DecideMove() extends Request
case class PrintText(text: String) extends Request