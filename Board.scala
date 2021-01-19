package L7

import scala.util.Random
// Description of board
// idx 0-5 are Player 1 houses
// idx 7-12 are Player 2 houses
// idx 6 is Player 1 end base
// idx 13 is Player 2 end base

class Board(initialStonesNumberInHouse: Int) {
  private var board = Array.fill(14) {initialStonesNumberInHouse}
  board(6) = 0
  board(13) = 0

  private def getOppositeIndex(idx: Int): Int =
    13 - (idx + 1)

  private def firstIndexWithStones(idx: Int): Int = {
    for(i <- 0 until 6) {
      if (board(idx + i) != 0) return idx + i
    }
    -1
  }

  def getBoard(): Array[Int] =
    board

  // method used by computers to automatic play
  def getRandomValidMove(isPlayer1Asking: Boolean): Int = {
    var validIndexes: List[Int] = List()
    var firstHouseIndex = 0
    if (! isPlayer1Asking) firstHouseIndex = 7

    for(i <- 0 until 6) {
      if (board(firstHouseIndex + i) != 0) {
        validIndexes = (firstHouseIndex + i) :: validIndexes
      }
    }
    if (validIndexes.length > 0) validIndexes(Random.nextInt(validIndexes.length))
    else -1
  }

  def getAdvantage(isPlayer1Asking: Boolean): Int = {
    if (isPlayer1Asking) board(6) - board(13)
    else board(13) - board(6)
  }

  def cloneBoard(): Board = {
    var cloneBoard = new Board(initialStonesNumberInHouse)
    cloneBoard.board = this.board.clone()
    cloneBoard
  }

  // first Boolean is true if this move was valid
  // second Boolean is true if player has next move
  def makeMove(playerNumber: Int, houseIndex: Int): (Boolean, Boolean) = {
    require(playerNumber == 1 || playerNumber == 2, "Wrong player's number")
    if (houseIndex == 6 || houseIndex == 13
      || (playerNumber == 1 && (houseIndex < 0 || houseIndex > 5))
      || (playerNumber == 2 && (houseIndex < 7 || houseIndex > 12))) return (false, false)

    val numberOfStones = board(houseIndex)
    if (numberOfStones == 0) return (false, false)

    board(houseIndex) = 0 // take stones from chosen house

    var currentIndex = houseIndex + 1
    for (i <- 0 until (numberOfStones - 1)) { // put one stone to every next hole except the last one
      if (currentIndex == 14) currentIndex = 0  // to go around the board
      board(currentIndex) = board(currentIndex) + 1
      currentIndex += 1
    }
    // put the last stone
    if (currentIndex == 14) currentIndex = 0  // to go around the board
    if (playerNumber == 1 && board(currentIndex) == 0 && currentIndex >= 0 && currentIndex <= 5) { // last stone ended in an empty hole of the player
      board(currentIndex) = 1
      board(6) = board(6) + board(getOppositeIndex(currentIndex)) // put stones from opposite hole to player's end base
      board(getOppositeIndex(currentIndex)) = 0
      (true, false)
    }
    else if (playerNumber == 2 && board(currentIndex) == 0 && currentIndex >= 7 && currentIndex <= 12) { // last stone ended in an empty hole of the player
      board(currentIndex) = 1
      board(13) = board(13) + board(getOppositeIndex(currentIndex)) // put stones from opposite hole to player's end base
      board(getOppositeIndex(currentIndex)) = 0
      (true, false)
    }
    else {
      board(currentIndex) = board(currentIndex) + 1 // put last stone in the hole
      if ((playerNumber == 1 && currentIndex == 6) || (playerNumber == 2 && currentIndex == 13))  // last stone ended in player's end base
        (true, true) // player has next move
      else (true, false)
    }
  }

  def getAllSelfStones(playerNumber: Int): Unit = {
    require(playerNumber == 1 || playerNumber == 2, "Wrong player's number")
    var endZoneIndex = 6
    var firstHouseIndex = 0
    if (playerNumber == 2) {
      endZoneIndex = 13
      firstHouseIndex = 7
    }
    for(i <- 0 until 6) {
      board(endZoneIndex) = board(endZoneIndex) + board(firstHouseIndex + i)
      board(firstHouseIndex + i) = 0
    }
  }

  def isEndOfGame(isPlayer1Turn: Boolean): Boolean = {
    var firstHouseIndex = 0
    if (! isPlayer1Turn) firstHouseIndex = 7
    for (i <- 0 until 6) {
      if (board(i) != 0) return false
    }
    true
  }

  // first Boolean is true if this move was valid
  // second Boolean is true if the game has ended -> player should make a move but didn't
  def updatePlayer(playerNumber: Int, houseIndex: Int): (Boolean, Boolean) = {
    require(playerNumber == 1 || playerNumber == 2, "Wrong player's number")
    if (houseIndex == 6 || houseIndex == 13
      || (playerNumber == 1 && (houseIndex < 0 || houseIndex > 5))
      || (playerNumber == 2 && (houseIndex < 7 || houseIndex > 12))) return (false, false)

    val numberOfStones = board(houseIndex)
    if (numberOfStones == 0) return (false, false)

    board(houseIndex) = 0 // take stones from chosen house
    var currentIndex = houseIndex + 1
    for (i <- 0 until (numberOfStones - 1)) { // put one stone to every next hole except the last one
      if (currentIndex == 14) currentIndex = 0  // to go around the board
      board(currentIndex) = board(currentIndex) + 1
      currentIndex += 1
    }
    // put the last stone
    if (currentIndex == 14) currentIndex = 0  // to go around the board
    if (playerNumber == 1 && board(currentIndex) == 0 && currentIndex >= 0 && currentIndex <= 5) { // last stone ended in an empty hole of the player
      board(currentIndex) = 1
      board(6) = board(6) + board(getOppositeIndex(currentIndex)) // put stones from opposite hole to player's end base
      board(getOppositeIndex(currentIndex)) = 0
      return (true, false)
    }
    else if (playerNumber == 2 && board(currentIndex) == 0 && currentIndex >= 7 && currentIndex <= 12) { // last stone ended in an empty hole of the player
      board(currentIndex) = 1
      board(13) = board(13) + board(getOppositeIndex(currentIndex)) // put stones from opposite hole to player's end base
      board(getOppositeIndex(currentIndex)) = 0
      return (true, false)
    }
    else {
      board(currentIndex) = board(currentIndex) + 1 // put last stone in the hole
      if (playerNumber == 1 && currentIndex == 6) { // last stone ended in player's end base
        val newHouseIdx = firstIndexWithStones(0)
        if (newHouseIdx != -1) updatePlayer(1, newHouseIdx)
        else {
          getAllSelfStones(2) // enemy collects all self stones
          (true, true)
        }
      }

      else if (playerNumber == 2 && currentIndex == 13) { // last stone ended in player's end base
        val newHouseIdx = firstIndexWithStones(7)
        if (newHouseIdx != -1) updatePlayer(2, newHouseIdx)
        else { // player 2 can't make any move
          getAllSelfStones(1) // enemy collects all self stones
          (true, true)
        }
      }
      else (true, false)
    }
  }

  def isEndOfGame(): Boolean = {
    board(6) + board(13) == initialStonesNumberInHouse * 12
  }

  def printBoard(): Unit = {
    System.out.println("Current board")
    System.out.print("player2: ")
    var idx = 12
    while (idx > 7) {
      System.out.print(board(idx) + "; ")
      idx -= 1
    }
    System.out.println(board(7))
    System.out.println("| " + board(13) + " |                        | " + board(6) +" |")
    System.out.print("player1: ")
    idx = 0
    while (idx < 5) {
      System.out.print(board(idx) + "; ")
      idx += 1
    }
    System.out.println(board(idx))
  }

  def printEndResults(): Unit = {
    System.out.println("The game has ended")
    System.out.println("Player 1 points: " + board(6))
    System.out.println("Player 2 points: " + board(13))
    if (board(6) > board(13)) System.out.println("Player 1 won.")
    else if (board(6) < board(13)) System.out.println("Player 2 won.")
    else System.out.println("The game ended in a tie.")
  }
}

