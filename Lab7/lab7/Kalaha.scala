package lab7

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}

object Kalaha extends App {
  val system: ActorSystem = ActorSystem("Kalaha")
  val reader: ActorRef = system.actorOf(Reader.props(system), "reader")

  val client1 = system.actorOf(Client.props(reader, "Zbigiew", Some(MinMaxAlgorithm)))
  val client2 = system.actorOf(Client.props(reader, "JSON"))

  val server = system.actorOf(Server.props(client1, client2), "server-actor")
}