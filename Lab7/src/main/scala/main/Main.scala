package main

import main.game.players.{Human, Player, RandomBot, SmartBot}
import akka.actor.{ActorRef, ActorSystem, PoisonPill, Props}
import main.server.{Client, Connect, Disconnect, Server}

object Main extends App {
    var p1: Player = null
    var p2: Player = null

    Interface.printMenu()
    Interface.menuOption match {
        case 1 => {
            p1 = new Human("human", 6, 6)
            p2 = new RandomBot("bot", 6, 6)
        }

        case 2 => {
            p1 = new Human("human", 6, 6)
            p2 = new SmartBot("bot", 6, 6)
        }

        case 3 => {
            p1 = new RandomBot("bot-1", 6, 6)
            p2 = new RandomBot("bot-2", 6, 6)
        }

        case 4 => {
            p1 = new SmartBot("bot-hard", 6, 6)
            p2 = new RandomBot("bot-easy", 6, 6)
        }

        case 5 => {
            p1 = new SmartBot("bot-1", 6, 6)
            p2 = new SmartBot("bot-2", 6, 6)
        }
    }

    val system = ActorSystem("kalaha")

    val server = system.actorOf(Props[Server](), "server")
    val c1 = system.actorOf(Props(classOf[Client], p1, true), "c1")
    val c2 = system.actorOf(Props(classOf[Client], p2, false), "c2")

    c1 ! Connect(server)
    c2 ! Connect(server)

    /*
        Thread.sleep(100)
        c2 ! Disconnect()
    */
}
