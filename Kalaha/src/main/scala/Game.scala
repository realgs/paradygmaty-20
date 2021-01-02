import scala.annotation.tailrec

//TODO smoothen the game logic
class Game private(numberOfPebbles: Int,board: Array[Int],private var turn: Int = 1) {

  //Makes a clone of the game for simulation purposes
  override def clone(): Game = {
    new Game(numberOfPebbles,board.clone(),turn)
  }

  //Makes a list of valid moves
  def validMoves():List[Int] = {
    @tailrec
    def validMovesLoop(current: Int, to: Int, acc: List[Int]):List[Int] = {
      if(current > to) acc
      else validMovesLoop(current + 1,to,if(board(current) > 0) current :: acc else acc)
    }
    if(turn % 2 == 1) validMovesLoop(0,5,List())
    else validMovesLoop(7,12,List())
  }

  //Makes a move on kalaha board(if move is invalid returns false)
  def move(chosenHole: Int): Boolean = {
    if (!belongsToPlayer(chosenHole)) false
    else if (isEmpty(chosenHole)) false
    else {
      var nextHole = (chosenHole + 1) % 14
      while (board(chosenHole) > 1) {
        if(nextHole == oppositeBase) nextHole = (nextHole + 1) % 14
        board(chosenHole) = board(chosenHole) - 1
        board(nextHole) = board(nextHole) + 1
        nextHole = (nextHole + 1) % 14
      }
      //Give the player additional move
      if(nextHole == oppositeBase) nextHole = (nextHole + 1) % 14
      if (nextHole == base) turn = turn - 1
      else if (belongsToPlayer(nextHole) && board(nextHole) == 0) captureOppositeStones(nextHole)
      turn = turn + 1
      board(chosenHole) = board(chosenHole) - 1
      board(nextHole) = board(nextHole) + 1
      true
    }
  }

  def getBoard: Array[Int] = board

  def getTurn: Int = turn

  def turnOfPlayer(player: Int):Boolean = 1 + (turn - 1) % 2 == player

  def declareWinner():Unit = {
    gatherPebbles()
    if(board(6) == board(13)) println("Game ended in a draw")
    else if(board(6) > board(13)) println("Player 1 won")
    else println("Player 2 won")
  }

  //Checks whether any of players have met game end condition
  def isGameOver: Boolean = {
    emptySide() || enoughPebblesInBase()
  }

  //Prints kalaha board in console
  def displayBoard(): Unit = {
    for (i <- 12 to 7 by -1) print(f"  ${board(i)}%02d")
    println()
    println(f"${board(13)}%02d                      ${board(6)}%02d")
    for (i <- 0 to 5) print(f"  ${board(i)}%02d")
    println()
  }

  //Takes pebbles from opposite base
  private def captureOppositeStones(hole: Int):Unit = {
    if (turn % 2 == 1) board(6) = board(6) + board(12 - hole)
    else board(13) = board(13) + board(12 - hole)
    board(12 - hole) = 0
  }

  //Returns current player's base
  private def base: Int = {
    if (turn % 2 == 1) 6 else 13
  }

  //Returns opposite player's base
  private def oppositeBase: Int = {
    if(turn % 2 == 1) 13 else 6
  }

  //Checks whether hole chosen by the player is empty
  private def isEmpty(chosenHole: Int): Boolean = board(chosenHole) == 0

  //Checks whether hole chosen by the player actually belongs to him
  private def belongsToPlayer(chosenHole: Int): Boolean = {
    turn % 2 == 1 && chosenHole >= 0 && chosenHole <= 5 || turn % 2 == 0 && chosenHole >= 7 && chosenHole <= 12
  }

  //Returns whether either of player's side is empty
  private def emptySide(): Boolean = {
    board.slice(0, 6).sum == 0 || board.slice(7, 13).sum == 0
  }

  //Returns all stones to corresponding player base
  private def gatherPebbles(): Unit = {
    board(6) = board(6) + board.slice(0, 6).sum
    board(13) = board(13) + board.slice(7, 13).sum
    for (i <- board.indices) if (i != 6 && i != 13) board(i) = 0
  }

  //Returns whether either of player's has enough pebbles in base to win
  private def enoughPebblesInBase(): Boolean = {
    board(6) > 6 * numberOfPebbles || board(13) > 6 * numberOfPebbles
  }
}

object Game {
  def apply(numberOfPebbles: Int): Game = {
    val initialBoardState = Array.fill(14)(numberOfPebbles)
    initialBoardState(6) = 0
    initialBoardState(13) = 0
    new Game(numberOfPebbles,initialBoardState)
  }
}
