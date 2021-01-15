package GameboardPackage

import GameboardPackage.Gameboard.{BASE_INDEX_PLAYER1, BASE_INDEX_PLAYER2,
  FIRST_INDEX_PLAYER1, FIRST_INDEX_PLAYER2,
  HOLES_IN_TABLE, PLAYER_1_ROUND,
  PLAYER_2_ROUND, createBoard} //This import was made in order not to use the object namespace
import scala.util.Random

/*
  Gameboard class is a class that has all Kahala methods that are necessary to run the game
*/

object Gameboard {
  private val FIRST_INDEX_PLAYER1 = 0
  private val BASE_INDEX_PLAYER1 = 6
  private val FIRST_INDEX_PLAYER2 = 7
  private val BASE_INDEX_PLAYER2 = 13
  private val HOLES_IN_TABLE = 14
  private val PLAYER_1_ROUND = 1
  private val PLAYER_2_ROUND = 2

  def createBoard(numberOfStones: Int): Array[Int] = {
    def createBoardHelp(currentIndex:Int, numberOfStones:Int): List[Int] = {
      if (currentIndex > BASE_INDEX_PLAYER2) Nil
      else if ((currentIndex+1)%7 == 0) 0 :: createBoardHelp(currentIndex+1,numberOfStones)
      else numberOfStones :: createBoardHelp(currentIndex+1,numberOfStones)
    }
    createBoardHelp(FIRST_INDEX_PLAYER1,numberOfStones).toArray
  }

  def drawWhoseMove(): Int = {
    val random = new Random
    val whoseTurn = random.nextInt(2) + 1
    whoseTurn
  }
}

class Gameboard(private var board: Array[Int], private var whoseRound: Int){

  def boardToList: List[Int] = {
    def boardToListHelp(currentIndex: Int): List[Int] = {
      if (currentIndex > BASE_INDEX_PLAYER2) Nil
      else board(currentIndex) :: boardToListHelp(currentIndex+1)
    }
    boardToListHelp(FIRST_INDEX_PLAYER1)
  }

  def boardClone(): Gameboard = {
    val temp = new Gameboard(board.clone(),whoseRound)
    temp
  }

  def getWhoseRound: Int =  {
    whoseRound
  }

  def getElem(index: Int): Int = {
    board(index)
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

  def calculateAdvantage(): Int = {
    if (whoseRound == PLAYER_1_ROUND) board(BASE_INDEX_PLAYER1) - board(BASE_INDEX_PLAYER2)
    else board(BASE_INDEX_PLAYER2) - board(BASE_INDEX_PLAYER1)
  }

  private def checkIfFieldCorrect(inputField: Int):Boolean = {
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
        if (stonesToGiveAway > 0) {
          if (whoseRound == PLAYER_1_ROUND) {
            if (currentIndex != BASE_INDEX_PLAYER2) {
              board(currentIndex) = board(currentIndex) + 1
              playerMoveHelp((currentIndex+1)%HOLES_IN_TABLE,stonesToGiveAway-1)
            } else playerMoveHelp(FIRST_INDEX_PLAYER1,stonesToGiveAway)
          } else {
            if (currentIndex != BASE_INDEX_PLAYER1) {
              board(currentIndex) = board(currentIndex) + 1
              playerMoveHelp((currentIndex+1)%HOLES_IN_TABLE,stonesToGiveAway-1)
            }
            else playerMoveHelp(FIRST_INDEX_PLAYER2,stonesToGiveAway)
          }
        } else {
          val indexOfLastMovedStone = if (currentIndex == 0) BASE_INDEX_PLAYER2 else currentIndex - 1
          lastStone(indexOfLastMovedStone)
          if (indexOfLastMovedStone == BASE_INDEX_PLAYER1 && whoseRound == PLAYER_1_ROUND) ()
          else if (indexOfLastMovedStone == BASE_INDEX_PLAYER2 && whoseRound == PLAYER_2_ROUND) ()
          else changePlayer()
        }
      }
      playerMoveHelp(inputField+1,stonesFromInputField)
    }
  }

  private def lastStone(indexOfLastMovedStone: Int): Unit = {
    if (whoseRound == PLAYER_1_ROUND) { //indexOfLastMovedStone 0 <= 5
      if (board(indexOfLastMovedStone) == 1 && indexOfLastMovedStone >= FIRST_INDEX_PLAYER1 && indexOfLastMovedStone < BASE_INDEX_PLAYER1) {
        val indexToStealFrom = BASE_INDEX_PLAYER2 - 1 - indexOfLastMovedStone
        board(BASE_INDEX_PLAYER1) += board(indexToStealFrom)
        board(indexToStealFrom) = 0
      }
    } else { //indexOfLastMovedStone 7 <= 12
      if (board(indexOfLastMovedStone) == 1 && indexOfLastMovedStone >= FIRST_INDEX_PLAYER2 && indexOfLastMovedStone < BASE_INDEX_PLAYER2) {
        val indexToStealFrom = BASE_INDEX_PLAYER2 - 1 - indexOfLastMovedStone
        board(BASE_INDEX_PLAYER2) += board(indexToStealFrom)
        board(indexToStealFrom) = 0
      }
    }
  }

  private def countPlayersStones(): (Int,Int) = {
    def countPlayersStonesHelp(currentIndex: Int,output: (Int,Int)): (Int,Int) = {
      if (currentIndex <= BASE_INDEX_PLAYER1)
        countPlayersStonesHelp(currentIndex+1,(output._1 + board(currentIndex),output._2))
      else if (currentIndex > BASE_INDEX_PLAYER1 && currentIndex <= BASE_INDEX_PLAYER2)
        countPlayersStonesHelp(currentIndex+1,(output._1,output._2 + board(currentIndex)))
      else output
    }
    countPlayersStonesHelp(0,(0,0))
  }

  private def stringPlayerRound(): String = {
    if (whoseRound == PLAYER_1_ROUND) "**************Player 1 Turn**************\n"
    else "**************Player 2 Turn**************\n"
  }

  def stringResult(): String = {
    val results = countPlayersStones()
    if (results._1 > results._2) s"PLAYER 1 WON THE GAME!!! \n ${results._1} > ${results._2}"
    else if (results._2 > results._1) s"PLAYER 2 WON THE GAME!!! \n ${results._2} > ${results._1}"
    else s"REMIS!!! \n ${results._1} = ${results._2}"
  }

  override def toString: String = {
    stringPlayerRound() +
      s"    [${board(12)}] - [${board(11)}] - [${board(10)}] - [${board(9)}] - [${board(8)}] - [${board(7)}]\n" +
      s"    [${board(13)}]                                    [${board(6)}]\n" +
      s"    [${board(0)}] - [${board(1)}] - [${board(2)}] - [${board(3)}] - [${board(4)}] - [${board(5)}]\n"
  }

  def testMove(): Unit = {
    val obj2 = new Gameboard(createBoard(4),PLAYER_1_ROUND)
    println("PLAYER CHOSES TO MOVE: " + 2 + " NOW RUN THE METHOD MOVE: \n")
    println(obj2.playerMove(2))
    println(obj2)
    println("PLAYER CHOSES TO MOVE: " + 1 + " NOW RUN THE METHOD MOVE: \n")
    println(obj2.playerMove(1))
    println(obj2)
    println("PLAYER CHOSES TO MOVE: " + 9 + " NOW RUN THE METHOD MOVE: \n")
    println(obj2.playerMove(9))
    println(obj2)
    println("PLAYER CHOSES TO MOVE: " + 7 + " NOW RUN THE METHOD MOVE: \n")
    println(obj2.playerMove(7))
    println(obj2)
    println("PLAYER CHOSES TO MOVE: " + 3 + " NOW RUN THE METHOD MOVE: \n")
    println(obj2.playerMove(3))
    println(obj2)
    println("**************")
    println("PLAYER CHOSES TO MOVE: " + 7 + " NOW RUN THE METHOD MOVE: \n")
    println(obj2.playerMove(7))
    println(obj2)
    println("PLAYER CHOSES TO MOVE: " + 2 + " NOW RUN THE METHOD MOVE: \n")
    println(obj2.playerMove(2))
    println(obj2)
    println(obj2.countPlayersStones())

    val obj3 = new Gameboard(createBoard(4),PLAYER_1_ROUND)
    obj3.changePlayer()
    println("PLAYER CHOSES TO MOVE: " + 9 + " NOW RUN THE METHOD MOVE: \n")
    println(obj3.playerMove(9))
    println(obj3)
    println("PLAYER CHOSES TO MOVE: " + 8 + " NOW RUN THE METHOD MOVE: \n")
    println(obj3.playerMove(8))
    println(obj3)
    println("PLAYER CHOSES TO MOVE: " + 2 + " NOW RUN THE METHOD MOVE: \n")
    println(obj3.playerMove(2))
    println(obj3)
    println("PLAYER CHOSES TO MOVE: " + 0 + " NOW RUN THE METHOD MOVE: \n")
    println(obj3.playerMove(0))
    println(obj3)
    println("PLAYER CHOSES TO MOVE: " + 10 + " NOW RUN THE METHOD MOVE: \n")
    println(obj3.playerMove(10))
    println(obj3)
    println("**************")
    println("PLAYER CHOSES TO MOVE: " + 0 + " NOW RUN THE METHOD MOVE: \n")
    println(obj3.playerMove(0))
    println(obj3)
    println("PLAYER CHOSES TO MOVE: " + 9 + " NOW RUN THE METHOD MOVE: \n")
    println(obj3.playerMove(9))
    println(obj3)
    (print(obj3.countPlayersStones()))
  }
}

