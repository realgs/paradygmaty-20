package scala

import akka.actor.{ActorRef, ActorSystem, Props}

import ServerRequest._

object Game extends App {
  val actorSystem: ActorSystem = ActorSystem("Kalaha")
  val clientOne: ActorRef = actorSystem.actorOf(Props(classOf[Client], Player.NPC_PLAYER_ONE))
  val clientTwo: ActorRef = actorSystem.actorOf(Props(classOf[Client], Player.NPC_PLAYER_TWO))
  val server: ActorRef = actorSystem.actorOf(Props(classOf[Server]))

  // npc vs npc
  server.tell(JOIN, clientOne)
  server.tell(JOIN, clientTwo)

  server ! START_GAME
}
