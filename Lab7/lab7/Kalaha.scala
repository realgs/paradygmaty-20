package lab7

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}

import scala.io.StdIn.readInt

object Kalaha extends App {
  val system: ActorSystem = ActorSystem("Kalaha")
  println("Witaj w symulatorze gry Kalaha")
  println("Wybierz, kto ma sterować posunięciami gracza pierwszego")
  showMenu()
  val input1 = readInt()
  println("Wybierz, kto ma sterować posunięciami gracza drugiego")
  showMenu()
  val input2 = readInt()

  val reader: ActorRef = system.actorOf(Reader.props(system), "reader")

  val client1 = createClient("Zbigniew", input1)
  val client2 = createClient("JSON", input2)

  val server = system.actorOf(Server.props(client1, client2), "server-actor")

  def showMenu(): Unit = {
    println("0 - ty lub twój kolega, za pomocą klawiatury")
    println("1 - Kamilek, niezbyt rozgarnięty algorytm")
    println("2 - Artur, algorytm na troszkę wyższym poziomie")
    println("3 - Zdzisiu, algorytm, który ci udowodni, że ty tej gry kompletnie nie rozumiesz")
  }

  def createClient(name: String, input: Int): ActorRef = {
    if (input >= 1 && input <= 3) system.actorOf(Client.props(reader, name, Some(new MinMaxAlgorithm(input * 7 - 5))))
    else system.actorOf(Client.props(reader, name))
  }
}
