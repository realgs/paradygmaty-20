class GameBoard {
  private val baseIndex: (Int, Int) = (6, 13)
  private var currentPlayer: Int = _
  private var oppositePlayer: Int = _
  private var currentHole: Int = _
  private var stones: Int = _
  private val board: Array[Int] = Array.fill[Int](14)(4)

  initBoard()

  def printBoard(): Unit = {
    print("Base 1 -> ")
    for (i <- baseIndex._2 until 6 by -1) {
      print("[" + board(i) + "] ")
    }
    println()
    print("\t\t\t   ")
    for (i <- 0 until baseIndex._1 + 1) {
      print("[" + board(i) + "] ")
    }
    println("<- Base 0")
  }

  def checkMove(hole: Int, player: Int): Boolean = {
    currentPlayer = player
    oppositePlayer = getOppositePlayer
    currentHole = getStartIndex(player) + hole
    if (board(currentHole) == 0)
      false
    else true
  }

  def checkResults() = {
    val player1Stones: Int = board(baseIndex._1)
    val player2Stones: Int = board(baseIndex._2)
    val winner: Int = if(player1Stones > player2Stones) 0 else 1

    val score: Int = Math.abs(player1Stones - player2Stones)
    println(s"Player $winner wins! Score: $score")
  }

  /*
  return 0 when game ends
  return 1 when currentPlayer has one more turn
  return 2 when nextPlayer has turn
   */

  def makeMove(): Int = {
    stones = board(currentHole)
    board(currentHole) = 0

    for (_ <- 0 until stones - 1) {
      currentHole = (currentHole + 1) % board.length
      if (currentHole == getBaseIndex(oppositePlayer))
        currentHole = (currentHole + 1) % board.length

      board(currentHole) += 1
    }

    currentHole = (currentHole + 1) % board.length
    board(currentHole) += 1 //adding last stone

    val gameStatus = checkGameStatus()
    if (gameStatus == 2) {
      /* checking position of last stone */
      if (currentHole == getBaseIndex(currentPlayer)) 1 //last stone is in base, one more turn
      else if (board(currentHole) == 1 && board(12 - currentHole) != 0 && currentHole < getBaseIndex(currentPlayer) && currentHole >= getStartIndex(currentPlayer)) { //last stone landed in empty hole, take all stones from opposite player to your base
        board(getBaseIndex(currentPlayer)) += board(12 - currentHole) + board(currentHole)
        board(12 - currentHole) = 0
        board(currentHole) = 0
        if(checkGameStatus() == 2) 2
        else 0
      }
      else 2
    }
    else {
      board(getBaseIndex(0)) += getAllStones(0)
      board(getBaseIndex(1)) += getAllStones(1)
      0
    }
  }

  /*
  return 0 when player 0 has empty holes
  return 1 when player 1 has empty holes
  return 2 when next round
   */
  private def checkGameStatus(): Int = {
    if (checkHoles(0)) {
      0
    }
    else if (checkHoles(1)) {
      1
    }
    else 2
  }

  private def checkHoles(player: Int): Boolean = {
    for (i <- getStartIndex(player) until getBaseIndex(player))
      if (board(i) != 0) return false
    true
  }

  private def getAllStones(player: Int): Int = {
    var allStones: Int = 0
    for (i <- getStartIndex(player) until getBaseIndex(player)) {
      allStones = allStones + board(i)
      board(i) = 0
    }
    allStones
  }

  private def getOppositePlayer: Int = {
    if (currentPlayer == 0) return 1
    0
  }

  private def getBaseIndex(player: Int): Int = {
    if (player == 0) return baseIndex._1
    baseIndex._2
  }

  private def getStartIndex(player: Int): Int = {
    if (player == 0) return 0
    7
  }

  private def initBoard(): Unit = {
    board(baseIndex._1) = 0
    board(baseIndex._2) = 0
  }
}
