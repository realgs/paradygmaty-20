import scala.io.StdIn
import akka.actor.{ActorSystem, Props}
object Main extends App {

  printMenu()

  val optionNumber = chooseOption()

  if(optionNumber == 1) playerVsComputerGame()
  else if(optionNumber == 2) computerVsComputerGame()
  else System.exit(0)


  def printMenu(): Unit = {

    println("# Menu")
    println("1. player vs computer")
    println("2. computer vs computer")
    println("3. exit\n")
  }

  def chooseOption(): Int = {

    var option: Int = 0
    var ifCorrectNumber: Boolean = false

    while(!ifCorrectNumber) {

      print("Select option: ")

      try {

        option = StdIn.readInt()

        ifCorrectNumber = option >= 1 && option <= 3

        if(!ifCorrectNumber)
          println("\nWrong number! Try again.")

      } catch {

        case _: NumberFormatException => println("\nWrong input! Try again.")
        case _: Exception => ()
      }
    }

    option
  }

  def playerVsComputerGame(): Unit = {

    val system = ActorSystem()
    val board = new Board
    val player1 = system.actorOf(Props(new HumanPlayer), "player1")
    val player2 = system.actorOf(Props(new ComputerPlayer), "player2")
    val server = system.actorOf(Props(new Server(player1, player2, board)), "server")

    server ! Server.AskForMove
  }
  def computerVsComputerGame(): Unit = {

    val system = ActorSystem()
    val board = new Board
    val player1 = system.actorOf(Props(new ComputerPlayer), "player1")
    val player2 = system.actorOf(Props(new ComputerPlayer), "player2")
    val server = system.actorOf(Props(new Server(player1, player2, board)), "server")

    server ! Server.AskForMove
  }
}
