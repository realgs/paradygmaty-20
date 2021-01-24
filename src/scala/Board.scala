package scala

import scala.Board.{AMOUNT_OF_PLAYER_HOLES, HOLES, PLAYER_ONE_BASE_INDEX, PLAYER_TWO_BASE_INDEX, STONES}
import scala.collection.mutable

object Board {
  val HOLES = 14
  val AMOUNT_OF_PLAYER_HOLES: Int = HOLES / 2
  val STONES = 6
  val PLAYER_ONE_BASE_INDEX = 6
  val PLAYER_TWO_BASE_INDEX = 13

  val instance = new Board
}

class Board {
  private val boardRepresentation: Array[Int] = new Array[Int](HOLES)

  for (i <- Range.inclusive(0, HOLES - 1)) {
    if (i != PLAYER_ONE_BASE_INDEX && i != PLAYER_TWO_BASE_INDEX) boardRepresentation(i) = STONES
  }

  def shouldPlayerRepeatTheMove: Boolean = shouldPlayerRepeatMove

  private var shouldPlayerRepeatMove: Boolean = false

  def playerOneAvailableHoles: List[Int] = {
    val queue: mutable.Queue[Int] = mutable.Queue()
    for (i <- 0 until PLAYER_ONE_BASE_INDEX) if (boardRepresentation(i) != 0) queue.enqueue(i + 1)
    queue.toList
  }

  def playerTwoAvailableHoles: List[Int] = {
    val queue: mutable.Queue[Int] = mutable.Queue()
    for (i <- PLAYER_ONE_BASE_INDEX + 1 until PLAYER_TWO_BASE_INDEX)
      if (boardRepresentation(i) != 0) queue.enqueue(i + 1 - AMOUNT_OF_PLAYER_HOLES)
    queue.toList
  }

  def amountOfStonesInFirstPlayerBase: Int = boardRepresentation(PLAYER_ONE_BASE_INDEX)

  def amountOfStonesInSecondPlayerBase: Int = boardRepresentation(PLAYER_TWO_BASE_INDEX)

  def shouldGameBeContinued: Boolean = {
    var shouldBeContinued = false
    for (i <- 0 until PLAYER_ONE_BASE_INDEX) if (boardRepresentation(i) != 0) shouldBeContinued = true
    if (!shouldBeContinued) {
      boardRepresentation(PLAYER_TWO_BASE_INDEX) = amountOfStonesInSecondPlayerBase + boardRepresentation.slice(PLAYER_ONE_BASE_INDEX + 1, PLAYER_TWO_BASE_INDEX).sum
      for (i <- PLAYER_ONE_BASE_INDEX + 1 until PLAYER_TWO_BASE_INDEX) boardRepresentation(i) = 0
      return false
    }

    shouldBeContinued = false
    for (i <- PLAYER_ONE_BASE_INDEX + 1 until PLAYER_TWO_BASE_INDEX) if (boardRepresentation(i) != 0) shouldBeContinued = true
    if (shouldBeContinued) true
    else {
      boardRepresentation(PLAYER_ONE_BASE_INDEX) = amountOfStonesInFirstPlayerBase + boardRepresentation.slice(0, PLAYER_ONE_BASE_INDEX).sum
      for (i <- 0 until PLAYER_ONE_BASE_INDEX) boardRepresentation(i) = 0
      false
    }
  }


  def selectHoleWithTheSmallestAmountOFStones(playerNumber: PlayerNumber): Int = {
    var index = 0
    var smallest = Integer.MAX_VALUE
    playerNumber match {
      case PlayerOne => for (i <- playerOneAvailableHoles) if (boardRepresentation(i - 1) < smallest) {
        smallest = boardRepresentation(i - 1)
        index = i
      }
      case PlayerTwo => for (i <- playerTwoAvailableHoles) if (boardRepresentation(i - 1 + AMOUNT_OF_PLAYER_HOLES) < smallest) {
        smallest = boardRepresentation(i - 1 + AMOUNT_OF_PLAYER_HOLES)
        index = i
      }
    }
    index
  }

  def printSituation(perspective: PlayerNumber): Unit = {
    perspective match {
      case PlayerOne =>
        print("   ")
        for (i <- PLAYER_TWO_BASE_INDEX - 1 to PLAYER_ONE_BASE_INDEX + 1 by -1) print(boardRepresentation(i) + " ")
        println()
        print(boardRepresentation(PLAYER_TWO_BASE_INDEX))
        for (_ <- Range.inclusive(0, HOLES)) print(" ")
        print(boardRepresentation(PLAYER_ONE_BASE_INDEX))
        println()
        print("   ")
        for (i <- Range.inclusive(0, PLAYER_ONE_BASE_INDEX - 1)) print(boardRepresentation(i) + " ")
        println()
      case PlayerTwo =>
        print("   ")
        for (i <- PLAYER_ONE_BASE_INDEX - 1 to 0 by -1) print(boardRepresentation(i) + " ")
        println()
        print(boardRepresentation(PLAYER_ONE_BASE_INDEX))
        for (_ <- Range.inclusive(0, HOLES)) print(" ")
        print(boardRepresentation(PLAYER_TWO_BASE_INDEX))
        println()
        print("   ")
        for (i <- Range.inclusive(PLAYER_ONE_BASE_INDEX + 1, PLAYER_TWO_BASE_INDEX - 1)) print(boardRepresentation(i) + " ")
        println()
    }
  }

  def takeFromHole(index: Int, playerNumber: PlayerNumber): Unit = {
    val excludesIndex: Int = if (playerNumber == PlayerOne) Board.PLAYER_TWO_BASE_INDEX else Board.PLAYER_ONE_BASE_INDEX

    val correctIndex = playerNumber match {
      case PlayerOne => index - 1
      case PlayerTwo => PLAYER_ONE_BASE_INDEX + index
    }

    val baseIndex = playerNumber match {
      case PlayerOne => PLAYER_ONE_BASE_INDEX
      case PlayerTwo => PLAYER_TWO_BASE_INDEX
    }

    var stonesToPut = boardRepresentation(correctIndex)
    boardRepresentation(correctIndex) = 0

    var i = (correctIndex + 1) % Board.HOLES
    while (stonesToPut > 0) {
      if (i != excludesIndex) {
        if (stonesToPut == 1) {
          if (boardRepresentation(i) == 0 && i != baseIndex && !playerNumber.holesNumbers.contains(i + 1)) {
            val temp = boardRepresentation((HOLES - 2 - i) % HOLES)
            boardRepresentation((HOLES - 2 - i) % HOLES) = 0
            boardRepresentation(baseIndex) += temp
          }
        }
        boardRepresentation(i) += 1
        stonesToPut -= 1
        if (stonesToPut == 0) {
          if (i == baseIndex) shouldPlayerRepeatMove = true
          else shouldPlayerRepeatMove = false
        }
      }
      i = (i + 1) % Board.HOLES
    }
  }
}
