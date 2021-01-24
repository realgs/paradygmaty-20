import akka.actor.{Actor, ActorRef, Timers}
import akka.util.Timeout
import scala.util.{Failure, Success}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import akka.pattern.ask

import scala.util.Random

class Server extends Actor with Timers {
  private val players: Array[ActorRef] = new Array[ActorRef](2)
  private var currentPlayer: Int = drawRound()
  private val gameBoard: GameBoard = new GameBoard(currentPlayer)

  implicit private val timeout: Timeout = Timeout(30.seconds)

  override def receive: Receive = onMessage(players)

  private def drawRound(): Int = Random.between(0, 2)

  private def nextPlayer(): Unit =
    currentPlayer match {
      case 1 => currentPlayer = 0
      case 0 => currentPlayer = 1
    }

  private def endGame() = context.system.terminate()

  private def onMessage(players: Array[ActorRef]): Receive = {
    case Connect(playerID: Int) =>
      players(playerID) = sender()
      context.watch(sender)
      if (!players.contains(null))
        startGame()

    case MakeMove() =>
      val chosenMove = players(currentPlayer) ? Move(gameBoard)

      chosenMove.onComplete {
        case Success(hole: Int) => executeMove(hole)
        case Failure(_) =>
          val randMove = randomMove()
          println(s"\nYour time is out, making random move: $randMove")
          executeMove(randMove)
      }
  }

  def randomMove(): Int = {
    val move = Random.between(0, 6)
    if (gameBoard.checkMove(move))
      move
    else randomMove()
  }

  def executeMove(move: Int): Unit = {
    val gameStatus = gameBoard.makeMove(move)
    gameStatus match {
      case 0 => gameOver()
      case 1 =>
        printBoard("One more round for Player ")
        self ! MakeMove()
      case 2 =>
        nextPlayer()
        printBoard("Next round for Player ")
        self ! MakeMove()
    }
  }

  private def printBoard(message: String): Unit = {
    Thread.sleep(1000)
    gameBoard.printBoard()
    Thread.sleep(1000)
    println(Console.BLUE + message + s"$currentPlayer" + Console.RESET)
  }

  private def gameOver() = {
    gameBoard.printBoard()
    println(Console.RED + "Game over!")
    gameBoard.finishGame()
    endGame()
  }

  private def startGame(): Unit = {
    println("Starting game!")
    Thread.sleep(1000)
    gameBoard.printBoard()
    Thread.sleep(1000)
    self ! MakeMove()
  }
}
