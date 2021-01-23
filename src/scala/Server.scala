package scala

import akka.actor.{Actor, ActorRef}

import scala.ServerRequest._
import scala.ClientRequest._
import scala.util.Random

class Server(var board: Board) extends Actor {
  var playerOne: ActorRef = null
  var playerTwo: ActorRef = null
  var alreadyConnected: Int = 0

  def start(): Unit = {
    println("Starting a game")
    val playerToStart = Random.between(1, 3)
    println(s"Player $playerToStart will start the game")
    printCurrentSituation(if (playerToStart == 1) PlayerOne else PlayerTwo)

    playerToStart match {
      case 1 => playerOne ! MOVE(board.playerOneAvailableHoles)
      case 2 => playerTwo ! MOVE(board.playerTwoAvailableHoles)
    }
  }

  def printCurrentSituation(perspective: PlayerNumber): Unit = {
    if (perspective == PlayerOne) println("Player one move")
    else if (perspective == PlayerTwo) println("Player two move")
    board.printSituation(perspective)
  }

  def join(): Unit = if (alreadyConnected == 0) {
    playerOne = sender()
    alreadyConnected = alreadyConnected + 1
  }
  else if (alreadyConnected == 1 && sender() != playerOne) {
    playerTwo = sender()
    alreadyConnected = alreadyConnected + 1
  }
  else println("Maximum number of two clients can be served")

  def takeFromHole(holeNumber: Int, playerNumber: PlayerNumber): Unit = {
    if (board.shouldGameBeContinued) {
      playerNumber match {
        case PlayerOne =>
          if (holeNumber >= 1 && holeNumber <= 6) {
            board.takeFromHole(holeNumber, PlayerOne)
            if (board.shouldGameBeContinued) {
              if (board.shouldPlayerRepeatTheMove) {
                printCurrentSituation(PlayerOne)
                playerOne ! MOVE(board.playerOneAvailableHoles)
              } else {
                printCurrentSituation(PlayerTwo)
                playerTwo ! MOVE(board.playerTwoAvailableHoles)
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
                playerTwo ! MOVE(board.playerTwoAvailableHoles)
              } else {
                printCurrentSituation(PlayerOne)
                playerOne ! MOVE(board.playerOneAvailableHoles)
              }
            }
            else self ! END_GAME
          }
      }
    }
    else endGame()
  }

  def endGame(): Unit = {
    if (board.amountOfHolesInFirstPlayerBase > board.amountOfHolesInSecondPlayerBase) println("Player one wins")
    else if (board.amountOfHolesInFirstPlayerBase == board.amountOfHolesInSecondPlayerBase) println("Result: draw")
    else println("Player two wins")
    context.system.terminate()
  }

  override def receive: Receive = {
    case START_GAME => start()
    case JOIN => join()
    case TAKE_FROM_HOLE(number, playerNumber) => if (board.shouldGameBeContinued) {
      playerNumber match {
        case PlayerOne =>
          if (board.playerOneAvailableHoles.contains(number)) takeFromHole(number, playerNumber)
          else playerOne ! MOVE(board.playerOneAvailableHoles)
        case PlayerTwo =>
          if (board.playerTwoAvailableHoles.contains(number)) takeFromHole(number, playerNumber)
          else playerTwo ! MOVE(board.playerTwoAvailableHoles)
      }
    }
    else endGame()
    case END_GAME => endGame()
  }
}
