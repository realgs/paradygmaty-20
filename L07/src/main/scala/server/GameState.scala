package server

object GameState {
  def apply(): GameState = new GameState()

  def from(gameState: GameState): GameState = {
    val newGameState = GameState()
    newGameState.playerTurn = gameState.playerTurn
    Array.copy(gameState.pits, 0, newGameState.pits, 0, gameState.cells)
    newGameState
  }

  def isGameFinished(state: GameState): Boolean = {
    def arePitsEmpty(left: Int, right: Int): Boolean = {
      for (i <- Range(left, right)) {
        if (state.pits(i) > 0) return false
      }
      true
    }

    if (state.playerTurn == "S") arePitsEmpty(0, 6) else arePitsEmpty(7, 13)
  }
}

class GameState {
  private var playerTurn = "S"
  private val pits = Array.fill(14)(6)
  pits(6) = 0
  pits(13) = 0
  private val cells = pits.length

  def points: (Int, Int) = (pits(6), pits(13))

  def playerPits(player: String): Array[Int] = {
    val arr = Array.fill(6)(0)
    if (player == "S") {
      Array.copy(pits, 0, arr, 0, 6)
    } else {
      Array.copy(pits, 7, arr, 0, 6)
    }
    arr
  }

  def nextTurn: String = playerTurn

  def skipTurn(): Unit = playerTurn = if (playerTurn == "S") "N" else "S"

  def play(player: String, pit: Int): GameState = {
    require(player == playerTurn)
    require(0 <= pit && pit < 6)

    val nextState = GameState.from(this)
    val pitIdx = indexOfPit(player, pit)

    require(pits(pitIdx) != 0)

    val seeds = nextState.pits(pitIdx)
    nextState.pits(pitIdx) = 0

    var i = 1
    var count = seeds
    while (i <= count) {
      if ((pitIdx + i) % nextState.pits.length != opponentsStoreIdx(player)) {
        nextState.pits((pitIdx + i) % nextState.pits.length) += 1
      } else {
        count += 1
      }
      i += 1
    }

    val lastPitIdx = (pitIdx + i - 1) % nextState.pits.length
    if (isPlayersPit(player, lastPitIdx) && nextState.pits(lastPitIdx) == 1) {
      val oppositePitIdx = 12 - lastPitIdx
      if (nextState.pits(oppositePitIdx) != 0) {
        if (player == "S") {
          nextState.pits(6) += nextState.pits(oppositePitIdx) + 1
        } else {
          nextState.pits(13) += nextState.pits(oppositePitIdx) + 1
        }
        nextState.pits(oppositePitIdx) = 0
        nextState.pits(lastPitIdx) = 0
      }
    }

    val nextPlayer = nextPlayerTurn(player, lastPitIdx)
    nextState.playerTurn = nextPlayer

    if (GameState.isGameFinished(nextState)) {
      val opponent = if (nextPlayer == "S") "N" else "S"
      if (opponent == "N") {
        for (i <- Range(0, 6)) {
          nextState.pits(13) += nextState.pits(12 - i)
          nextState.pits(12 - i) = 0
        }
      } else {
        for (i <- Range(0, 6)) {
          nextState.pits(6) += nextState.pits(i)
          nextState.pits(i) = 0
        }
      }
    }

    nextState
  }

  private def indexOfPit(player: String, pit: Int): Int = {
    (if (player == "S") 0 else 1) * 7 + pit
  }

  private def nextPlayerTurn(player: String, lastPitIdx: Int): String = {
    if (player == "S") {
      if (lastPitIdx == 6) "S" else "N"
    } else {
      if (lastPitIdx == 13) "N" else "S"
    }
  }

  private def isPlayersPit(player: String, idx: Int): Boolean = {
    if (player == "S") 0 <= idx && idx < 6 else 7 <= idx && idx < 13
  }

  private def opponentsStoreIdx(player: String): Int = {
    if (player == "S") 13 else 6
  }

  private def getWinner: String = {
    val (sp, np) = points
    if (sp < np) "N"
    else if (sp > np) "S"
    else "X"
  }

  override def toString: String = toString("")

  def toString(player: String = "", finished: Boolean = false): String = {
    val cellSize = 6
    val sep = "-" * cellSize
    val line = ("+" + sep) * 6 + "+"
    val emptySpace = " " * cellSize

    def pitsToString(getPitFun: Int => Int): String = {
      val str = new StringBuilder()
      str.append("|" + emptySpace)
      for (i <- Range(0, 6)) {
        str.append(s"| %${cellSize - 2}d ".format(getPitFun(i)))
      }
      str.append("|" + emptySpace + "|")
      str.toString
    }

    val northLine = {
      val getPit: Int => Int = {
        if (player == "N") (idx: Int) => pits(5 - idx)
        else (idx: Int) => pits(12 - idx)
      }
      pitsToString(getPit)
    }

    val midLine = {
      val msg = {
        if (player == "") ""
        else if (finished) {
          val winner = getWinner
          if (winner == "X") "Draw"
          else if (player == winner) "You won"
          else "You lost"
        } else {
          if (player == playerTurn) "Your turn"
          else "Opponent's turn"
        }
      }
      val str = new StringBuilder()
      str.append(s"| %${cellSize - 2}d |%${cellSize * 6 + 5}s| %${cellSize - 2}d |"
        .format(
          if (player == "N") pits(6) else pits(13),
          Utils.centerString(msg, cellSize * 6 + 5, ' '),
          if (player == "N") pits(13) else pits(6)))
      str.toString
    }

    val southLine = {
      val getPit: Int => Int = {
        if (player == "N") (idx: Int) => pits(7 + idx)
        else (idx: Int) => pits(idx)
      }
      pitsToString(getPit)
    }

    val board = new StringBuilder()
    board.append(s"+$sep$line$sep+\n")
    board.append(s"$northLine\n")
    board.append(s"|$emptySpace$line$emptySpace|\n")
    board.append(s"$midLine\n")
    board.append(s"|$emptySpace$line$emptySpace|\n")
    board.append(s"$southLine\n")
    board.append(s"+$sep$line$sep+\n")
    board.toString
  }
}
