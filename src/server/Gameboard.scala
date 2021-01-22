package pl.pawelklecha.kalaha
package server

import players.Player
import players.Player.PlayerType
import server.Gameboard._

class Gameboard {

  private val gameboard: Array[Int] = Array.fill(14)(DEFAULT_NUMBER_OF_STONES)
  private var currentPlayer = Player.PLAYER_1

  gameboard(PLAYER1_BASE_INDEX) = 0
  gameboard(PLAYER2_BASE_INDEX) = 0

  override def clone(): Gameboard = {
    val cloned = new Gameboard()

    for (i <- 0 until GAMEBOARD_SIZE) {
      cloned.gameboard(i) = gameboard(i)
    }

    cloned.currentPlayer = currentPlayer

    cloned
  }

  def getGameboard: Array[Int] = gameboard

  def getCurrentPlayer: PlayerType = currentPlayer

  def checkEndGameStatus(): Boolean = currentPlayer match {
    case Player.PLAYER_1 => gameboard.slice(PLAYER1_FIRST_INDEX, PLAYER1_BASE_INDEX).sum == 0 && currentPlayer == Player.PLAYER_1
    case Player.PLAYER_2 => gameboard.slice(PLAYER2_FIRST_INDEX, PLAYER2_BASE_INDEX).sum == 0 && currentPlayer == Player.PLAYER_2
    case _ => false
  }

  def checkMoveCorrectness(hole: Int): Boolean = {
    currentPlayer match {
      case Player.PLAYER_1 =>
        if (hole < PLAYER1_FIRST_INDEX || hole > PLAYER1_BASE_INDEX - 1) false
        else if (gameboard(hole) == 0) false
        else true

      case Player.PLAYER_2 =>
        if (hole < PLAYER2_FIRST_INDEX || hole > PLAYER2_BASE_INDEX - 1) false
        else if (gameboard(hole) == 0) false
        else true
    }
  }

  def nextPlayer(): Unit = {
    currentPlayer = currentPlayer match {
      case Player.PLAYER_1 => Player.PLAYER_2
      case Player.PLAYER_2 => Player.PLAYER_1
    }
  }

  def makeMove(hole: Int): Unit = {
    val stones = gameboard(hole)
    var currentIndexToPlaceStone = hole

    for (_ <- 0 until stones) {
      currentIndexToPlaceStone = (currentIndexToPlaceStone + 1) % GAMEBOARD_SIZE

      currentPlayer match {
        case Player.PLAYER_1 => if (currentIndexToPlaceStone == PLAYER2_BASE_INDEX) currentIndexToPlaceStone = PLAYER1_FIRST_INDEX
        case Player.PLAYER_2 => if (currentIndexToPlaceStone == PLAYER1_BASE_INDEX) currentIndexToPlaceStone += 1
      }

      gameboard(currentIndexToPlaceStone) += 1
    }

    if (checkTakeOpponentStonesPossibility(currentIndexToPlaceStone)) {
      takeOpponentStones(currentIndexToPlaceStone)
    } else if (!checkFreeMoveAvailability(currentIndexToPlaceStone)) {
      nextPlayer()
    }

    gameboard(hole) = 0
  }

  def calculateAdvantage(player: PlayerType): Int = {
    player match {
      case Player.PLAYER_1 => gameboard(PLAYER1_BASE_INDEX) - gameboard(PLAYER2_BASE_INDEX)
      case Player.PLAYER_2 => gameboard(PLAYER2_BASE_INDEX) - gameboard(PLAYER1_BASE_INDEX)
    }
  }

  def printResult(): Unit = {
    for (i <- PLAYER1_FIRST_INDEX until PLAYER1_BASE_INDEX) gameboard(PLAYER1_BASE_INDEX) += gameboard(i)
    for (i <- PLAYER2_FIRST_INDEX until PLAYER2_BASE_INDEX) gameboard(PLAYER2_BASE_INDEX) += gameboard(i)
    val results = (gameboard(PLAYER1_BASE_INDEX), gameboard(PLAYER2_BASE_INDEX))

    println(
      if (results._1 > results._2) s"** PLAYER 1 WON **\n ${results._1} > ${results._2}"
      else if (results._2 > results._1) s"** PLAYER 2 WON **\n ${results._2} > ${results._1}"
      else s"** DRAW **\n ${results._1} = ${results._2}"
    )
  }

  def printGameBoard(): Unit = {
    println(
      s"************** ${currentPlayer.toString} Turn **************\n" +
        s"    [${gameboard(12)}] - [${gameboard(11)}] - [${gameboard(10)}] - [${gameboard(9)}] - [${gameboard(8)}] - [${gameboard(7)}]\n" +
        s"    [${gameboard(13)}]                          [${gameboard(6)}]\n" +
        s"    [${gameboard(0)}] - [${gameboard(1)}] - [${gameboard(2)}] - [${gameboard(3)}] - [${gameboard(4)}] - [${gameboard(5)}]\n"
    )
  }

  private def checkTakeOpponentStonesPossibility(lastHolePosition: Int): Boolean = {
    currentPlayer match {
      case Player.PLAYER_1 => (PLAYER1_FIRST_INDEX until PLAYER1_LAST_INDEX contains lastHolePosition) && gameboard(lastHolePosition) == 1
      case Player.PLAYER_2 => (PLAYER2_FIRST_INDEX until PLAYER2_LAST_INDEX contains lastHolePosition) && gameboard(lastHolePosition) == 1
    }
  }

  private def takeOpponentStones(lastHolePosition: Int): Unit = {
    val oppositeHolePosition = PLAYER1_BASE_INDEX + (PLAYER1_BASE_INDEX - lastHolePosition)

    currentPlayer match {
      case Player.PLAYER_1 => if ((PLAYER1_FIRST_INDEX until PLAYER1_LAST_INDEX contains lastHolePosition) && gameboard(lastHolePosition) == 1) {
        gameboard(PLAYER1_BASE_INDEX) += gameboard(oppositeHolePosition) + gameboard(lastHolePosition)
      }
      case Player.PLAYER_2 => if ((PLAYER2_FIRST_INDEX until PLAYER2_LAST_INDEX contains lastHolePosition) && gameboard(lastHolePosition) == 1) {
        gameboard(PLAYER2_BASE_INDEX) += gameboard(oppositeHolePosition) + gameboard(lastHolePosition)
      }
    }

    gameboard(oppositeHolePosition) = 0
    gameboard(lastHolePosition) = 0
  }

  private def checkFreeMoveAvailability(lastHolePosition: Int): Boolean = {
    currentPlayer match {
      case Player.PLAYER_1 => lastHolePosition == PLAYER1_BASE_INDEX
      case Player.PLAYER_2 => lastHolePosition == PLAYER2_BASE_INDEX
    }
  }
}

object Gameboard {
  private val DEFAULT_NUMBER_OF_STONES = 4

  val GAMEBOARD_SIZE = 14

  val PLAYER1_FIRST_INDEX = 0
  val PLAYER1_LAST_INDEX = 5
  val PLAYER1_BASE_INDEX = 6

  val PLAYER2_FIRST_INDEX = 7
  val PLAYER2_LAST_INDEX = 12
  val PLAYER2_BASE_INDEX = 13
}
