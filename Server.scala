import akka.actor.Actor

import scala.util.Random

case object Start
case class AskForApproval(number:Int)
case object GiveApproval
case object DoNotGiveApproval

case class MakeMove(numberOfZone:Int)
case object CorrectMove
case object InCorrectMove

case object TimeIsOut
case object WrongFormat

case object GameOver

class Server(private val game:Game) extends Actor
{
  override def receive: Receive =
  {
    case _ if game.isGameOver() => sendInformationAboutGameOver()

    case AskForApproval(number) => controlPermission(number)

    case MakeMove(numberOfZone) => controlMove(numberOfZone)

    case TimeIsOut|WrongFormat => randomChoice()
  }

  private def sendInformationAboutGameOver():Unit =
  {
    println(s"Koniec gry wynik: ${if(game.getWinner() == 0)"remis" else "wygrana gracza " + game.getWinner()}")
    sender ! GameOver
  }

  private def controlPermission(number:Int):Unit =
  {
    if (number == game.getIndexOfTurnPlayer())
    {
      fakeClearScreen()
      game.displayBoard()
      sender ! GiveApproval
    }
    else sender ! DoNotGiveApproval
  }

  private def controlMove(numberOfZone:Int):Unit =
  {
    if (game.move(numberOfZone)) sender ! CorrectMove
    else
    {
      println("Niepoprawne pole spróbuj jeszcze raz")
      sender ! InCorrectMove
    }
  }

  private def randomChoice():Unit =
  {
    println(s"\nGracz nie wykonał ruch w ciągu 30 sekund lub wprowadził błedy symbol, wylosowano pole - ${getRandomZone()}")
    sender ! CorrectMove
  }

  private def fakeClearScreen(): Unit =
  {
     Thread.sleep(1000)
     print("\n" * 10)
  }

  private def getRandomZone():Int =
  {
    val validMoves = game.validMovesForPlayer()
    var numberOfZone = validMoves(Random.nextInt(validMoves.length))

    while(game.move(numberOfZone)) numberOfZone = validMoves(Random.nextInt(validMoves.length))
    numberOfZone
  }
}

object Server
{
  def apply(game:Game):Server = new Server(game)
}
