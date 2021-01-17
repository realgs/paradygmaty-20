package actors.server

import actors.player.PlayerActions
import actors.server.Server._
import akka.actor.{Actor, ActorRef, PoisonPill, Props, Timers}
import akka.pattern.ask
import akka.util.Timeout
import gameboard.GameBoard
import model.Player

import scala.concurrent.duration.DurationInt

class Server extends Actor with Timers {

  private val playersList = new Array[ActorRef](2)
  private var gameBoard: GameBoard = _

  private var timeout = false

  override def receive: Receive = {
    case ServerActions.ConnectToServer(playerId: Int) => {
      playersList(playerId) = sender()
      println(s"Player ${playerId + 1} has connected to the server")

      if(playersList(PLAYER_ONE_INDEX) != null && playersList(PLAYER_TWO_INDEX) != null) {
        self ! ServerActions.StartGame
      }
    }

    case ServerActions.StartGame => {
      println("Game is starting")

      gameBoard = new GameBoard()
      playersList(PLAYER_ONE_INDEX) ! PlayerActions.MakeMove(gameBoard.clone())

      startTimer()
    }

    case ServerActions.ValidateMove(holeIndex: Int) => {
      println("Server is validating the move")

      if (!timeout) {
        println("No timeout")
        if (gameBoard.isMoveValid(holeIndex)) handleValidMove(holeIndex)
        else handleInvalidMove(holeIndex)
      }
      else {
        timeout = false
      }
    }

    case ServerActions.NextMove => {
      startTimer()
      playersList(gameBoard.getActualTurn.id) ! PlayerActions.MakeMove(gameBoard.clone())
    }

    case ServerActions.GameOver => {
      gameBoard.finishGame()
      printWinnerCommunicate()
      disconnectPlayers()
    }

    case ServerActions.Timeout => {
      println("Timeout occurred")
      timeout = true
      playersList(gameBoard.getActualTurn.id) ! PlayerActions.Timeout(gameBoard.clone())
    }
  }

  private def handleValidMove(holeIndex: Int): Unit = {
    println("Handle Valid move")
    stopTimer()
    gameBoard.makeMove(holeIndex)

    if (gameBoard.isGameOver) {
      self ! ServerActions.GameOver
    } else {
      self ! ServerActions.NextMove
    }
  }

  private def handleInvalidMove(holeIndex: Int): Unit = {
    println("Handle invalid move")
    println(s"Hole index: ${holeIndex}")
    sender() ! PlayerActions.InvalidMove(holeIndex)
  }

  private def printWinnerCommunicate(): Unit = {
    val finalScore = gameBoard.getFinalScore(Player.First)

    if (finalScore > 0) println("Player 1 has won!")
    else if (finalScore < 0) println("Player 2 has won!")
    else println("Draw!")
  }

  private def disconnectPlayers(): Unit = {
    for (i <- playersList.indices) {
      playersList(i) ! PoisonPill
    }
  }

  private def startTimer(): Unit = {
    timers.startSingleTimer(TIMER_KEY, ServerActions.Timeout, TURN_TIME)
  }

  private def stopTimer(): Unit = {
    timers.cancel(TIMER_KEY)
  }
}

object Server {
  def props: Props = Props[Server]()

  private val TURN_TIME = 30000.millis

  private val PLAYER_ONE_INDEX = 0
  private val PLAYER_TWO_INDEX = 1

  private val TIMER_KEY = "SERVER_TIMER"
}
