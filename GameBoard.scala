class GameBoard(startingPlayer: Int) {
  private val baseIndex: (Int, Int) = (6, 13)
  private var currentPlayer: Int = startingPlayer
  private var oppositePlayer: Int = getOpponent(currentPlayer)
  private var currentHole: Int = _
  private val board: Array[Int] = Array.fill[Int](14)(4)

  initBoard()

  def printBoard(): Unit = {
    print(Console.RED + "Base 1 -> ")
    for (i <- baseIndex._2 until 6 by -1) {
      print("[" + board(i) + "] ")
    }
    println()
    print("\t\t\t  ")
    for (i <- 0 until baseIndex._1 + 1) {
      print("[" + board(i) + "] ")
    }
    println("<- Base 0" + Console.RESET)
  }

  def copy(): GameBoard = {
    val gameBoard: GameBoard = new GameBoard(currentPlayer)
    gameBoard.currentHole = currentHole

    for (i <- board.indices) {
      gameBoard.board(i) = board(i)
    }

    gameBoard
  }

  def getCurrentPlayer: Int = currentPlayer

  def checkMove(hole: Int): Boolean = {
    if (board(getStartIndex(currentPlayer) + hole) == 0)
      false
    else true
  }

  def finishGame(): Unit = {
    val player1Stones: Int = board(baseIndex._1)
    val player2Stones: Int = board(baseIndex._2)

    val winner: Int =
      if (player1Stones > player2Stones) 0
      else if (player1Stones < player2Stones) 1
      else -1

    winner match {
      case -1 =>
        println("Remis. :)")
      case w =>
        val score: Int = Math.abs(player1Stones - player2Stones)
        println(s"Player $w wins! Score: $score")
    }
  }

  /*
  return 0 when game ends
  return 1 when currentPlayer has one more turn
  return 2 when nextPlayer has turn
   */
  def makeMove(move: Int): Int = {
    currentHole = getStartIndex(currentPlayer) + move
    val stones = board(currentHole)
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

    gameStatus match {
      case 2 =>
        if (ifOneMoreTurn()) return 1 //last stone is in base, one more turn
        if (ifCanStealStones()) { //last stone landed in empty hole, take all stones from opposite player to your base
          stealStones()

          if (checkGameStatus() != 2) {
            sumUpStonesToBase()
            return 0
          }
        }
        nextRound()
        2
      case _ =>
        sumUpStonesToBase()
        0
    }
  }

  /*
  return 0 when player 0 has empty holes
  return 1 when player 1 has empty holes
  return 2 when next round
   */
  def checkGameStatus(): Int = {
    if (checkHoles(0)) return 0
    if (checkHoles(1)) return 1
    2
  }

  private def ifOneMoreTurn(): Boolean = {
    if (currentHole == getBaseIndex(currentPlayer)) return true
    false
  }

  private def ifCanStealStones(): Boolean = {
    if (currentHole < getBaseIndex(currentPlayer) && currentHole >= getStartIndex(currentPlayer))
      if (board(currentHole) == 1 && board(12 - currentHole) != 0)
        return true
    false
  }

  private def stealStones(): Unit = {
    board(getBaseIndex(currentPlayer)) += board(12 - currentHole) + board(currentHole)
    board(12 - currentHole) = 0
    board(currentHole) = 0
  }

  def nextRound(): Unit = {
    val tmp = currentPlayer
    currentPlayer = oppositePlayer
    oppositePlayer = tmp
  }

  private def checkHoles(player: Int): Boolean = {
    for (i <- getStartIndex(player) until getBaseIndex(player))
      if (board(i) != 0) return false
    true
  }

  private def sumUpStonesToBase(): Unit = {
    board(getBaseIndex(0)) += getAllStones(0)
    board(getBaseIndex(1)) += getAllStones(1)
  }

  def getAllStones(player: Int): Int = {
    var allStones: Int = 0
    for (i <- getStartIndex(player) until getBaseIndex(player)) {
      allStones = allStones + board(i)
      board(i) = 0
    }
    allStones
  }

  def getBaseScore(player: Int): Int = {
    board(getBaseIndex(player)) - board(getBaseIndex(getOpponent(player)))
  }

  def getFinalScore(player: Int): Int = {
    board(getBaseIndex(player)) + getAllStones(player)
  }

  private def getOpponent(player: Int): Int = {
    player match {
      case 1 => 0
      case 0 => 1
    }
  }

  def getBaseIndex(player: Int): Int = {
    player match {
      case 0 => baseIndex._1
      case 1 => baseIndex._2
    }
  }

  def getStartIndex(player: Int): Int = {
    player match {
      case 0 => 0
      case 1 => 7
    }
  }

  private def initBoard(): Unit = {
    board(baseIndex._1) = 0
    board(baseIndex._2) = 0
  }
}
