package GameboardPackage

import GameboardPackage.Gameboard.{BASE_INDEX_PLAYER1, BASE_INDEX_PLAYER2, FIRST_INDEX_PLAYER1, FIRST_INDEX_PLAYER2, PLAYER_1_ROUND, PLAYER_2_ROUND}

import scala.annotation.tailrec
import scala.util.Random

object Gameboard {
  private val FIRST_INDEX_PLAYER1 = 0
  private val BASE_INDEX_PLAYER1 = 6
  private val FIRST_INDEX_PLAYER2 = 7
  private val BASE_INDEX_PLAYER2 = 13
  private val PLAYER_1_ROUND = 1
  private val PLAYER_2_ROUND = 2
}

class Gameboard {
  val board = createBoard(14)
  private var whoseRound = 1

  def createBoard(numberOfStones: Int): Array[Int] = {
    def createBoardHelp(currentIndex:Int, numberOfStones:Int): List[Int] = {
      if (currentIndex > BASE_INDEX_PLAYER2) Nil
      else if ((currentIndex+1)%7 == 0) 0 :: createBoardHelp(currentIndex+1,numberOfStones)
      else numberOfStones :: createBoardHelp(currentIndex+1,numberOfStones)
    }
    createBoardHelp(FIRST_INDEX_PLAYER1,numberOfStones).toArray
  }

  def drawWhoseRound(): Int = {
    val random = new Random()
    random.nextInt(2)
  }

  def changePlayer(): Unit ={
    if (whoseRound == PLAYER_1_ROUND) whoseRound = PLAYER_2_ROUND
    else whoseRound = PLAYER_1_ROUND
  }

  def endGameCheck(): Boolean = {
    if (board.slice(FIRST_INDEX_PLAYER1,BASE_INDEX_PLAYER1).sum == 0 && whoseRound == PLAYER_1_ROUND) true
    else if (board.slice(FIRST_INDEX_PLAYER2,BASE_INDEX_PLAYER2).sum == 0 && whoseRound == PLAYER_2_ROUND) true
    else false
  }

  def checkIfFieldCorrect(inputField: Int):Boolean = {
    if (whoseRound == PLAYER_1_ROUND) {
      if (inputField < FIRST_INDEX_PLAYER1 || inputField > BASE_INDEX_PLAYER1 - 1) false
      else if (board(inputField) == 0) false
      else true
    } else {
      if (inputField < FIRST_INDEX_PLAYER2 || inputField > BASE_INDEX_PLAYER2 - 1) false
      else if (board(inputField) == 0) false
      else true
    }
  }

  def playerMove(inputField: Int): Unit = {
    if (checkIfFieldCorrect(inputField)) {
      val stonesFromInputField = board(inputField)
      board(inputField) = 0
      def playerMoveHelp(currentIndex: Int, stonesToGiveAway: Int): Unit = {
        if (stonesToGiveAway > 0){
          if (whoseRound == PLAYER_1_ROUND) {
            if (currentIndex != BASE_INDEX_PLAYER2) board(currentIndex) = board(currentIndex) + 1
          } else {
            if (currentIndex != BASE_INDEX_PLAYER1) board(currentIndex) = board(currentIndex) + 1
          }
          playerMoveHelp((currentIndex+1)%14,stonesToGiveAway-1)
        }
      }
      playerMoveHelp(inputField+1,stonesFromInputField)
    }
  }
}
