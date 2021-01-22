package pl.pawelklecha.kalaha

import events.ServerActionEvent
import players.Player
import players.computer.ComputerPlayer
import players.real.RealPlayer
import server.{Gameboard, Server}

import akka.actor.{ActorRef, ActorSystem, Props}

import scala.io.StdIn

object KalahaGame extends App {
  println("*************** Choose game type ***************")
  println("1. Human vs. Human")
  println("2. Human vs. Computer")
  println("3. Computer vs. Computer")

  var gameType = -1

  while (gameType < 1 || gameType > 4) {
    println("Choose a number: ")
    try {
      gameType = StdIn.readInt()
    } catch {
      case _: NumberFormatException => gameType = -1
    }
  }

  val system = ActorSystem("KalahaGame")
  val gameboard: Gameboard = new Gameboard
  var player1: ActorRef = _
  var player2: ActorRef = _
  var server: ActorRef = _

  gameType match {
    case 1 =>
      player1 = system.actorOf(Props(classOf[RealPlayer]), "player1")
      player2 = system.actorOf(Props(classOf[RealPlayer]), "player2")
    case 2 =>
      player1 = system.actorOf(Props(classOf[RealPlayer]), "player1")
      player2 = system.actorOf(Props(classOf[ComputerPlayer], Player.PLAYER_2), "player2")
    case 3 =>
      player1 = system.actorOf(Props(classOf[ComputerPlayer], Player.PLAYER_1), "player1")
      player2 = system.actorOf(Props(classOf[ComputerPlayer], Player.PLAYER_2), "player2")
  }

  server = system.actorOf(Props(classOf[Server], player1, player2, gameboard), "server")
  server ! ServerActionEvent.NextMove
}
