package scala

import akka.actor.{Actor, ActorRef}

import scala.ServerRequest._
import scala.ClientRequest._
import scala.util.Random

class Server(var board: Board) extends Actor {
  var playerOne: ActorRef = null
  var playerTwo: ActorRef = null
  var alreadyConnected: Int = 0

  private def printStartInfoAndReturnStartingPlayerNumber(): Int = {
    println("Starting a game")
    val playerToStart = Random.between(1, 3)
    println(s"Player $playerToStart will start the game")
    printCurrentSituation(if (playerToStart == 1) PlayerOne else PlayerTwo)
    playerToStart
  }

  private def start(): Unit = {
    val playerToStart = printStartInfoAndReturnStartingPlayerNumber()
    playerToStart match {
      case 1 => playerOne ! MOVE
      case 2 => playerTwo ! MOVE
    }
  }

  private def printCurrentSituation(perspective: PlayerNumber): Unit = {
    if (perspective == PlayerOne) println("Player one move")
    else if (perspective == PlayerTwo) println("Player two move")
    board.printSituation(perspective)
  }

  private def join(): Unit =
    if (alreadyConnected == 0) {
      playerOne = sender()
      alreadyConnected = alreadyConnected + 1
    }
    else if (alreadyConnected == 1 && sender() != playerOne) {
      playerTwo = sender()
      alreadyConnected = alreadyConnected + 1
    }
    else println("Maximum number of two clients can be served")

  private def takeFromHole(holeNumber: Int, playerNumber: PlayerNumber): Unit = {
    if (board.shouldGameBeContinued) {
      playerNumber match {
        case PlayerOne =>
          if (holeNumber >= 1 && holeNumber <= 6) {
            board.takeFromHole(holeNumber, PlayerOne)
            if (board.shouldGameBeContinued) {
              if (board.shouldPlayerRepeatTheMove) {
                printCurrentSituation(PlayerOne)
                playerOne ! MOVE
              } else {
                printCurrentSituation(PlayerTwo)
                playerTwo ! MOVE
              }
            }
            else self ! END_GAME
          }
        case PlayerTwo =>
          if (holeNumber >= 1 && holeNumber <= 6) {
            board.takeFromHole(holeNumber, PlayerTwo)
            if (board.shouldGameBeContinued) {
              if (board.shouldPlayerRepeatTheMove) {
                printCurrentSituation(PlayerTwo)
                playerTwo ! MOVE
              } else {
                printCurrentSituation(PlayerOne)
                playerOne ! MOVE
              }
            }
            else self ! END_GAME
          }
      }
    }
    else self ! END_GAME
  }

  private def tryGetCorrectNumber(number: Int, playerNumber: PlayerNumber): Unit = {
    playerNumber match {
      case PlayerOne =>
        if (board.playerOneAvailableHoles.contains(number)) takeFromHole(number, playerNumber)
        else playerOne ! MOVE
      case PlayerTwo =>
        if (board.playerTwoAvailableHoles.contains(number)) takeFromHole(number, playerNumber)
        else playerTwo ! MOVE
    }
  }

  private def endGame(): Unit = {
    if (board.amountOfStonesInFirstPlayerBase > board.amountOfStonesInSecondPlayerBase) println("Player one wins")
    else if (board.amountOfStonesInFirstPlayerBase == board.amountOfStonesInSecondPlayerBase) println("Result: draw")
    else println("Player two wins")
    context.system.terminate()
  }

  override def receive: Receive = {
    case START_GAME => start()
    case JOIN => join()
    case TAKE_FROM_HOLE(number, playerNumber) =>
      if (board.shouldGameBeContinued) {
        tryGetCorrectNumber(number, playerNumber)
      }
      else
        self ! END_GAME
    case END_GAME => endGame()
  }
}
