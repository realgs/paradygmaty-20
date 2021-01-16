import GameBoard.{HOLES_BOARD_NUMBER, PLAYER_ONE_BASE_INDEX, PLAYER_ONE_FIRST_HOLE_INDEX, PLAYER_TWO_BASE_INDEX, PLAYER_TWO_FIRST_HOLE_INDEX, STONES_INITIAL_AMOUNT}

class GameBoard {
  private var turn = Turn.FirstPlayer
  private val board: Array[Int] = Array.fill(HOLES_BOARD_NUMBER)(STONES_INITIAL_AMOUNT)

  private val printer = BoardPrinter(board)

  board(PLAYER_ONE_BASE_INDEX) = 0
  board(PLAYER_TWO_BASE_INDEX) = 0

  def makeMove(holeIndex: Int): Unit = {
    var actualIndex = holeIndex

    for (i <- 0 until board(holeIndex)) {
      actualIndex = (actualIndex + 1) % HOLES_BOARD_NUMBER

      turn match {
        case Turn.FirstPlayer => if (actualIndex == PLAYER_TWO_BASE_INDEX) actualIndex = 0
        case Turn.SecondPlayer => if (actualIndex == PLAYER_ONE_BASE_INDEX) actualIndex += 1
      }

      board(actualIndex) += 1
    }

    if (isTakingOppositeStonesAvailable(actualIndex)) {
      takeOppositeStones(actualIndex)
    }

    if (!isNextMoveAvailable(actualIndex)) {
      changeTurn()
    }

    // TODO funkcja od klonowania planszy (ale to do zastanowienia) myslalem o tym w kontekscie komunikacji miedzy aktorami

    board(holeIndex) = 0
  }

  def isGameOver: Boolean = {
    var stonesSum = 0

    turn match {
      case Turn.FirstPlayer =>
        for (i <- PLAYER_ONE_FIRST_HOLE_INDEX until PLAYER_ONE_BASE_INDEX) {
          stonesSum += board(i)
        }

      case Turn.SecondPlayer =>
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

  def getActualTurn: Turn.Value = turn

  def getActualScore(turn: Turn.Value): Int = turn match {
    case Turn.FirstPlayer => board(PLAYER_ONE_BASE_INDEX)
    case Turn.SecondPlayer => board(PLAYER_TWO_BASE_INDEX)
  }

  def getBoard: Array[Int] = board

  def printBoard(): Unit = printer.printBoard(turn)

  def printBoard(turn: Turn.Value): Unit = printer.printBoard(turn)

  private def isNextMoveAvailable(lastStoneHoleIndex: Int): Boolean = turn match {
    case Turn.FirstPlayer => lastStoneHoleIndex == PLAYER_ONE_BASE_INDEX
    case Turn.SecondPlayer => lastStoneHoleIndex == PLAYER_TWO_BASE_INDEX
  }

  // first we check if last move was in player's board part
  // then we check if this hole has only one stone
  private def isTakingOppositeStonesAvailable(lastStoneHoleIndex: Int): Boolean = turn match {
    case Turn.FirstPlayer =>
      (PLAYER_ONE_FIRST_HOLE_INDEX until PLAYER_ONE_BASE_INDEX contains lastStoneHoleIndex) && board(lastStoneHoleIndex) == 1

    case Turn.SecondPlayer =>
      (PLAYER_TWO_FIRST_HOLE_INDEX until PLAYER_TWO_BASE_INDEX contains lastStoneHoleIndex) && board(lastStoneHoleIndex) == 1
  }

  private def takeOppositeStones(lastStoneHoleIndex: Int): Unit = {
    var oppositeHoleIndex: Int = 0

    turn match {
      case Turn.FirstPlayer =>
        oppositeHoleIndex = PLAYER_ONE_BASE_INDEX + (PLAYER_ONE_BASE_INDEX - lastStoneHoleIndex)
        board(PLAYER_ONE_BASE_INDEX) += board(oppositeHoleIndex) + board(lastStoneHoleIndex)

      case Turn.SecondPlayer =>
        oppositeHoleIndex = PLAYER_ONE_BASE_INDEX - (PLAYER_ONE_BASE_INDEX - lastStoneHoleIndex)
        board(PLAYER_TWO_BASE_INDEX) += board(oppositeHoleIndex) + board(lastStoneHoleIndex)
    }

    board(oppositeHoleIndex) = 0
    board(lastStoneHoleIndex) = 0
  }

  private def changeTurn(): Unit = turn match {
    case Turn.FirstPlayer => turn = Turn.SecondPlayer
    case Turn.SecondPlayer => turn = Turn.FirstPlayer
  }
}

object GameBoard {
  val PLAYERS_HOLES_NUMBER = 6
  val STONES_INITIAL_AMOUNT = 6

  val PLAYER_ONE_FIRST_HOLE_INDEX = 0
  val PLAYER_ONE_BASE_INDEX: Int = PLAYERS_HOLES_NUMBER

  val PLAYER_TWO_FIRST_HOLE_INDEX: Int = PLAYERS_HOLES_NUMBER + 1
  val PLAYER_TWO_BASE_INDEX: Int = PLAYERS_HOLES_NUMBER * 2 + 1

  val HOLES_BOARD_NUMBER: Int = PLAYERS_HOLES_NUMBER * 2 + 2
}
