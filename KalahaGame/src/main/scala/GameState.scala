class GameState(var seedsPerHouse: Int = 4, val southName: String = "South", val northName: String = "North") {
  assert(seedsPerHouse >= 0)

  val BOARD_SIZE = 14
  val NORTH_STORE_INDEX = 13
  val SOUTH_STORE_INDEX = 6

  var board: Array[Int] = Array.fill(BOARD_SIZE)(seedsPerHouse)
  board(6) = 0
  board(13) = 0

  // Returns true if player should be given another move, false otherwise
  def move(relativeIndex: Int)(turn: Int): Boolean = {
    val index = if (turn == 0) south(relativeIndex) else north(relativeIndex)
    val toIncrement = toPopulate(index, board(index))(turn)

    val lastCapture: Boolean = canCapture(toIncrement.last)(turn)
    val lastStore: Boolean = isPlayerStore(toIncrement.last)(turn)

    board(index) = 0
    toIncrement.foreach(i => board(i) += 1)

    if (lastCapture) {
      capture(toIncrement.last)(turn)
      return true
    }

    lastStore
  }

  def getAnyCorrectMove(turn: Int): Int = {
    for (i <- 5 to 0) {
      if (isMoveCorrect(i)(turn)) {
        i
      }
    }
    -1
  }

  def isMoveCorrect(relativeIndex: Int)(turn: Int): Boolean = {
    try {
      assert(relativeIndex >= 0 && relativeIndex < 6)
      assert(board(toGeneralIndex(relativeIndex)(turn)) > 0)
      true
    } catch {
      case _: AssertionError => false
    }
  }

  // Returns array of indices of houses which are to be incremented when moving from index with size seeds
  private def toPopulate(index: Int, size: Int)(turn: Int): Array[Int] = {
    assert(size > 0, "Illegal move size")
    assert(index != SOUTH_STORE_INDEX && index != NORTH_STORE_INDEX, "Tried to move from a store")

    // Omit the opponents store
    val toOmit = if (turn == 0) NORTH_STORE_INDEX else SOUTH_STORE_INDEX

    val firstIndex = (index + 1) % (BOARD_SIZE)
    val range = firstIndex until (firstIndex + size)

    var result = range.toArray.map(i => i % BOARD_SIZE)

    if (result.contains(toOmit)) {
      result = result.filter(_ != toOmit).appended((result.last + 1) % BOARD_SIZE)
    }

    result
  }

  // Returns true if last seed fulfills some condition for next move to be taken
  private def canCapture(lastIndex: Int)(turn: Int): Boolean = {
    if (board(lastIndex) == 0 && lastIndex != SOUTH_STORE_INDEX && lastIndex != NORTH_STORE_INDEX &&
      board(oppositeIndex(lastIndex)) != 0) {
      if (turn == 0) return (0 to 5).contains(lastIndex)
      else return (7 to 12).contains(lastIndex)
    }
    false
  }

  private def capture(index: Int)(turn: Int): Unit = {
    val targetStore = if (turn == 0) SOUTH_STORE_INDEX else NORTH_STORE_INDEX
    board(targetStore) += board(oppositeIndex(index))
    board(oppositeIndex(index)) = 0
  }

  private def isPlayerStore(index: Int)(turn: Int): Boolean = {
    if (turn == 0) index == SOUTH_STORE_INDEX else index == NORTH_STORE_INDEX
  }

  def checkGameEnded: Boolean = {
    if (southSideCount() == 0) {
      for (i <- SOUTH_STORE_INDEX + 1 until NORTH_STORE_INDEX) {
        board(SOUTH_STORE_INDEX) += board(i)
        board(i) = 0
      }
      return true
    }

    if (northSideCount() == 0) {
      for (i <- 0 until SOUTH_STORE_INDEX) {
        board(NORTH_STORE_INDEX) += board(i)
        board(i) = 0
      }
      return true
    }

    false
  }

  def getWinner: Int = {
    if (board(SOUTH_STORE_INDEX) > board(NORTH_STORE_INDEX)) {
      // South is the winner
      0
    } else if (board(SOUTH_STORE_INDEX) < board(NORTH_STORE_INDEX)) {
      // North is the winner
      1
    } else {
      // Draw
      -1
    }
  }

  private def toGeneralIndex(relativeIndex: Int)(turn: Int): Int = {
    if (turn == 0) south(relativeIndex) else north(relativeIndex)
  }

  private def oppositeIndex(index: Int): Int = {
    if (index == 6) 13 else if (index == 13) 6 else 12 - index
  }

  private def south(i: Int): Int = {
    assert(i >= 0 && i <= 6)
    i
  }

  private def north(i: Int): Int = {
    assert(i >= 0 && i <= 5)
    7 + i
  }

  private def southSideCount(): Int = {
    board.slice(0, SOUTH_STORE_INDEX).sum
  }

  private def northSideCount(): Int = {
    board.slice(7, NORTH_STORE_INDEX).sum
  }

  def getScore(turn: Int): Int = {
    if (turn == 0) board(SOUTH_STORE_INDEX) else board(NORTH_STORE_INDEX)
  }

  def deepCopy: GameState = {
    val cpy = GameState(seedsPerHouse, southName, northName)
    cpy.board = board.clone()
    cpy
  }

  override def toString: String = {
    val delimiter = "   "
    s"       <--- $northName\n ------------------------    \n  ${board.slice(7, 13).reverse.mkString("", delimiter, "")}    \n                             \n  ${board(13)}                   ${board(6)}    \n                            \n  ${board.slice(0, 6).mkString("", delimiter, "")}      \n ------------------------     \n         $southName --->"
  }
}

object GameState {
  def apply(): GameState = new GameState(0)

  def apply(seedsPerHouse: Int = 4, southName: String = "South", northName: String = "North"): GameState =
    new GameState(seedsPerHouse, southName, northName)
}
