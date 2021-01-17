import akka.actor.ActorSystem
import akka.actor.Props
object Main extends App {
  val system = ActorSystem()
  val server = system.actorOf(Props(classOf[Server],Game(4)))
  val firstPlayer = system.actorOf(Props(classOf[Client],server,AI(1,15)))
  val secondPlayer = system.actorOf(Props(classOf[Client],server,Human("Jakub",2)))
  firstPlayer ! Start
  secondPlayer ! Start
}
