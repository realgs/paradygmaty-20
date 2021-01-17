package gameboard

import model.GameConstants._
import model.Player

class GameBoard {
  private var turn = Player.First
  private val board: Array[Int] = Array.fill(HOLES_BOARD_NUMBER)(STONES_INITIAL_AMOUNT)

  private val printer = BoardPrinter(board)

  board(PLAYER_ONE_BASE_INDEX) = 0
  board(PLAYER_TWO_BASE_INDEX) = 0

  def makeMove(holeIndex: Int): Unit = {
    var actualIndex = holeIndex

    for (_ <- 0 until board(holeIndex)) {
      actualIndex = (actualIndex + 1) % HOLES_BOARD_NUMBER

      turn match {
        case Player.First => if (actualIndex == PLAYER_TWO_BASE_INDEX) actualIndex = 0
        case Player.Second => if (actualIndex == PLAYER_ONE_BASE_INDEX) actualIndex += 1
      }

      board(actualIndex) += 1
    }

    if (isTakingOppositeStonesAvailable(actualIndex)) {
      takeOppositeStones(actualIndex)
    }

    if (!isNextMoveAvailable(actualIndex)) {
      changeTurn()
    }

    board(holeIndex) = 0
  }

  def isMoveValid(holeIndex: Int): Boolean = {
    if (board(holeIndex) == 0) return false

    turn match {
      case Player.First => PLAYER_ONE_FIRST_HOLE_INDEX until PLAYER_ONE_BASE_INDEX contains holeIndex
      case Player.Second => PLAYER_TWO_FIRST_HOLE_INDEX until PLAYER_TWO_BASE_INDEX contains holeIndex
    }
  }


  def isGameOver: Boolean = {
    var stonesSum = 0

    turn match {
      case Player.First =>
        for (i <- PLAYER_ONE_FIRST_HOLE_INDEX until PLAYER_ONE_BASE_INDEX) {
          stonesSum += board(i)
        }

      case Player.Second =>
        for (i <- PLAYER_TWO_FIRST_HOLE_INDEX until PLAYER_TWO_BASE_INDEX) {
          stonesSum += board(i)
        }
    }

    stonesSum == 0
  }

  // when game is over we need to return all stones to players bases
  def finishGame(): Unit = {
    for (i <- PLAYER_ONE_FIRST_HOLE_INDEX until PLAYER_ONE_BASE_INDEX) {
      board(PLAYER_ONE_BASE_INDEX) += board(i)
    }

    for (i <- PLAYER_TWO_FIRST_HOLE_INDEX until PLAYER_TWO_BASE_INDEX) {
      board(PLAYER_TWO_BASE_INDEX) += board(i)
    }
  }

  def getActualScore(player: Player.Value): Int = player match {
    case Player.First => board(PLAYER_ONE_BASE_INDEX) - board(PLAYER_TWO_BASE_INDEX)
    case Player.Second => board(PLAYER_TWO_BASE_INDEX) - board(PLAYER_ONE_BASE_INDEX)
  }

  def getFinalScore(player: Player.Value): Int = {
    finishGame()
    getActualScore(player)
  }

  def getActualTurn: Player.Value = turn

  def getBoard: Array[Int] = board

  def printBoard(): Unit = printer.printBoard(turn)

  def printBoard(player: Player.Value): Unit = printer.printBoard(player)

  override def clone(): GameBoard = {
    val cloned = new GameBoard()

    for (i <- 0 until HOLES_BOARD_NUMBER) {
      cloned.board(i) = this.board(i)
    }

    cloned.turn = turn

    cloned
  }

  private def isNextMoveAvailable(lastStoneHoleIndex: Int): Boolean = turn match {
    case Player.First => lastStoneHoleIndex == PLAYER_ONE_BASE_INDEX
    case Player.Second => lastStoneHoleIndex == PLAYER_TWO_BASE_INDEX
  }

  // first we check if last move was in player's board part
  // then we check if this hole has only one stone
  private def isTakingOppositeStonesAvailable(lastStoneHoleIndex: Int): Boolean = turn match {
    case Player.First =>
      (PLAYER_ONE_FIRST_HOLE_INDEX until PLAYER_ONE_BASE_INDEX contains lastStoneHoleIndex) && board(lastStoneHoleIndex) == 1

    case Player.Second =>
      (PLAYER_TWO_FIRST_HOLE_INDEX until PLAYER_TWO_BASE_INDEX contains lastStoneHoleIndex) && board(lastStoneHoleIndex) == 1
  }

  private def takeOppositeStones(lastStoneHoleIndex: Int): Unit = {
    var oppositeHoleIndex: Int = 0

    turn match {
      case Player.First =>
        oppositeHoleIndex = PLAYER_ONE_BASE_INDEX + (PLAYER_ONE_BASE_INDEX - lastStoneHoleIndex)
        board(PLAYER_ONE_BASE_INDEX) += board(oppositeHoleIndex) + board(lastStoneHoleIndex)

      case Player.Second =>
        oppositeHoleIndex = PLAYER_ONE_BASE_INDEX - (PLAYER_ONE_BASE_INDEX - lastStoneHoleIndex)
        board(PLAYER_TWO_BASE_INDEX) += board(oppositeHoleIndex) + board(lastStoneHoleIndex)
    }

    board(oppositeHoleIndex) = 0
    board(lastStoneHoleIndex) = 0
  }

  private def changeTurn(): Unit = turn match {
    case Player.First => turn = Player.Second
    case Player.Second => turn = Player.First
  }
}
