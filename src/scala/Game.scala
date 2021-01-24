package scala

import akka.actor.{ActorRef, ActorSystem, Props}

import ServerRequest._

object Game extends App {

  val actorSystem: ActorSystem = ActorSystem("Kalaha")
  val board: Board = Board.instance
  val server: ActorRef = actorSystem.actorOf(Props(classOf[Server], board))

  var playerOne: ActorRef = null
  var playerTwo: ActorRef = null

  println("Choose player one: ")
  println("1.Random NPC")
  println("2.Smarter NPC")
  println("3.Human player")
  println("Choice: ")
  Helper.getInputFromUser match {
    case 1 => playerOne = actorSystem.actorOf(Props(classOf[Client], new NPCPlayer(PlayerOne)))
    case 2 => playerOne = actorSystem.actorOf(Props(classOf[Client], new IntelligentNPCPlayer(PlayerOne, board)))
    case 3 => playerOne = actorSystem.actorOf(Props(classOf[Client], new HumanPlayer(PlayerOne)))
  }
  println("Choose player two: ")
  println("1.Random NPC")
  println("2.Smarter NPC")
  println("3.Human player")
  println("Choice: ")
  Helper.getInputFromUser match {
    case 1 => playerTwo = actorSystem.actorOf(Props(classOf[Client], new NPCPlayer(PlayerTwo)))
    case 2 => playerTwo = actorSystem.actorOf(Props(classOf[Client], new IntelligentNPCPlayer(PlayerTwo, board)))
    case 3 => playerTwo = actorSystem.actorOf(Props(classOf[Client], new HumanPlayer(PlayerTwo)))
  }

  server.tell(JOIN, playerOne)
  server.tell(JOIN, playerTwo)
  server ! START_GAME
}
