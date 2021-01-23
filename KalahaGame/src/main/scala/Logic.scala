object Logic {
  val TREE_DEPTH = 15

  def getMove(gs: GameState)(turn: Int): Int = {
    var maxValue = Integer.MIN_VALUE

    val alpha = Integer.MIN_VALUE
    val beta = Integer.MAX_VALUE

    var chosenMove = 0

    for (moveIndex <- 0 until 6) {
      if (gs.isMoveCorrect(moveIndex)(turn)) {
        val boardCopy = gs.deepCopy
        boardCopy.move(moveIndex)(turn)
        val result = miniMax(boardCopy, TREE_DEPTH, alpha, beta, maximizingPlayer = true)(turn)
        if (result > maxValue) {
          maxValue = result
          chosenMove = moveIndex
        }
      }
    }

    chosenMove
  }

  def miniMax(board: GameState, depth: Int, a: Int, b: Int, maximizingPlayer: Boolean)(turn: Int): Int = {
    val opponentIdent = (turn + 1) % 2

    var alpha = a
    var beta = b

    if (depth == 0 || board.checkGameEnded) {
      return board.getScore(turn) - board.getScore(opponentIdent)
    }

    if (maximizingPlayer) {
      var value = Integer.MIN_VALUE

      var moveIndex = 0
      while (moveIndex < 6 && beta > alpha) {
        if (board.isMoveCorrect(moveIndex)(turn)) {
          val boardCopy = board.deepCopy
          boardCopy.move(moveIndex)(turn)

          value = Math.max(value, miniMax(boardCopy, depth - 1, alpha, beta, maximizingPlayer = false)(opponentIdent))
          alpha = Math.max(alpha, value)
        }
        moveIndex += 1
      }
      value
    }
    else {
      // Minimizing player
      var value = Integer.MAX_VALUE

      var moveIndex = 0
      while (moveIndex < 6 && beta > alpha) {
        if (board.isMoveCorrect(moveIndex)(turn)) {
          val boardCopy = board.deepCopy
          boardCopy.move(moveIndex)(turn)

          value = Math.min(value, miniMax(boardCopy, depth - 1, alpha, beta, maximizingPlayer = true)(opponentIdent))
          beta = Math.min(alpha, value)
        }
        moveIndex += 1
      }
      value
    }
  }
}
