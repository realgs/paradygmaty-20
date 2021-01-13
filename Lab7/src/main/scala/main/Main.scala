package main

import main.game.players.{Human, RandomBot, SmartBot}
import akka.actor.{ActorRef, ActorSystem, PoisonPill, Props}
import main.server.{Client, Connect, Disconnect, Server}

object Main extends App {
    val p1 = new SmartBot("p1", 6, 6)
    val p2 = new RandomBot("p2", 6, 6)

    val system = ActorSystem("kalaha")

    val server = system.actorOf(Props[Server](), "server")
    val c1 = system.actorOf(Props(classOf[Client], p1, true), "c1")
    val c2 = system.actorOf(Props(classOf[Client], p2, false), "c2")

    c1 ! Connect(server)
    c2 ! Connect(server)

//    Thread.sleep(100)
//
//    c1 ! Disconnect()

}
