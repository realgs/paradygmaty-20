import akka.actor.{ActorRef, ActorSystem, Props}

object Main extends App {
  val system = ActorSystem("Kalaha")
  val server = system.actorOf(Props[Server], "Server")
  var player1: ActorRef = _
  var player2: ActorRef = _

  runGame()

  def runGame() = {
    printWelcome()
    val mode = chooseOption()

    mode match {
      case 1 =>
        player1 = system.actorOf(Props(classOf[Player], server, 0), "Player_1")
        player2 = system.actorOf(Props(classOf[Player], server, 1), "Player_2")
      case 2 =>
        player1 = system.actorOf(Props(classOf[Player], server, 0), "Player_1")
        player2 = system.actorOf(Props(classOf[Computer], server, 1), "Player_2")
      case 3 =>
        player1 = system.actorOf(Props(classOf[Computer], server, 0), "Player_1")
        player2 = system.actorOf(Props(classOf[Computer], server, 1), "Player_2")
      case _ =>
        system.terminate()
    }
  }

  private def printWelcome(): Unit = {
    println("Welcome in super hiper extra game - KALAHA")
    println("Please select an option: ")
    println("[1] Player vs player")
    println("[2] Player vs computer (random)")
    println("[3] Computer vs computer (random)")
    println("[Any] I don't know what I am doing here, get out!")
  }

  private def chooseOption(): Int = {
    scala.io.StdIn.readInt()
  }
}
