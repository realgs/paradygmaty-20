import akka.actor.ActorSystem
import akka.actor.Props
object Main extends App {
  val system = ActorSystem()
  val server = system.actorOf(Props(classOf[Server],Game(6)))
  val firstPlayer = system.actorOf(Props(classOf[Client],server,1))
  val secondPlayer = system.actorOf(Props(classOf[Client],server,2))
  firstPlayer ! Start
  secondPlayer ! Start
}
