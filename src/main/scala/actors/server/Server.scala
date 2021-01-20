package actors.server

import actors.player.PlayerActions
import actors.server.Server._
import akka.actor.{Actor, ActorRef, PoisonPill, Props, Timers}
import akka.pattern.ask
import akka.util.Timeout
import gameboard.GameBoard
import model.GameConstants.NUMBER_OF_PLAYERS
import model.Player

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

class Server extends Actor with Timers {

  private val playersList = new Array[ActorRef](NUMBER_OF_PLAYERS)
  private var gameBoard: GameBoard = _

  implicit private val timeout: Timeout = Timeout(TURN_TIME)

  override def receive: Receive = {
    case ServerActions.ConnectToServer(playerId: Int) => {
      playersList(playerId) = sender()
      println(s"Player ${playerId + 1} has connected to the server")

      if (!playersList.contains(null)) startGame()
    }

    case ServerActions.ValidateMove(holeIndex: Int) => {
      sender ! gameBoard.isMoveValid(holeIndex)
    }

    case ServerActions.NextMove =>
      printTurnInformation()

      val future = playersList(gameBoard.getActualTurn.id) ? PlayerActions.MakeMove(gameBoard.clone())

      future onComplete {
        case Success(holeIndex: Int) => handleValidMove(holeIndex)
        case Failure(_) => handleTimeout()
      }

    case ServerActions.GameOver =>
      gameBoard.finishGame()
      printWinnerCommunicate()
      disconnectPlayers()
      terminateActorsSystem()
  }

  private def startGame(): Unit = {
    gameBoard = new GameBoard()
    self ! ServerActions.NextMove
  }

  private def handleValidMove(holeIndex: Int): Unit = {
    gameBoard.makeMove(holeIndex)

    if (gameBoard.isGameOver) {
      self ! ServerActions.GameOver
    } else {
      self ! ServerActions.NextMove
    }
  }

  private def handleTimeout(): Unit = {
    println("Timeout! You lost!")
    disconnectPlayers()
    terminateActorsSystem()
  }

  private def printWinnerCommunicate(): Unit = {
    val finalScore = gameBoard.getFinalScore(Player.First)

    if (finalScore > 0) println("Player 1 has won!")
    else if (finalScore < 0) println("Player 2 has won!")
    else println("Draw!")
  }

  private def printTurnInformation(): Unit = {
    println(s"Player ${gameBoard.getActualTurn.id + 1} turn")
    gameBoard.printBoard()
  }

  private def disconnectPlayers(): Unit = {
    for (i <- playersList.indices) {
      playersList(i) ! PoisonPill
    }
  }

  private def terminateActorsSystem() = context.system.terminate()
}

object Server {
  def props: Props = Props[Server]()

  private val TURN_TIME = 30.seconds
}
