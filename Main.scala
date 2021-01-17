
import lab7.{CompPlayer, HumanPlayer, Server, StartGame}
import akka.actor.{ActorRef, ActorSystem, Props}
object Main extends App {
    val ourSystem = ActorSystem("ourSystem")
    val server: ActorRef = ourSystem.actorOf(Server.props())
    val Player1: ActorRef = ourSystem.actorOf(Props(classOf[CompPlayer],0,server))
    val Player2: ActorRef = ourSystem.actorOf(Props(classOf[HumanPlayer],1,server))
    Thread.sleep(100)
    server ! StartGame()
}


