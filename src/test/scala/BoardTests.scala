import gameboard.GameBoard
import model.{GameConstants, Player}
import org.scalatest.FunSuite

class BoardTests extends FunSuite {

  test("takeOppositeStonesTest") {
    val gameBoard = new GameBoard()
    val board = gameBoard.getBoard

    board(1) = 1
    board(2) = 0
    board(10) = 20

    gameBoard.makeMove(1)

    assert(board(GameConstants.PLAYER_ONE_BASE_INDEX) == 21)
  }

  test("nextTurnAvailableTest") {
    val gameBoard = new GameBoard()
    val board = gameBoard.getBoard

    board(5) = 1
    gameBoard.makeMove(5)

    assert(gameBoard.getActualTurn == Player.First)
  }

  test("isGameOverTest") {
    val gameBoard = new GameBoard()
    val board = gameBoard.getBoard

    for(i <- GameConstants.PLAYER_ONE_FIRST_HOLE_INDEX until GameConstants.PLAYER_ONE_BASE_INDEX) {
      board(i) = 0
    }

    assert(gameBoard.isGameOver)
  }

  test("finishGameTest") {
    val gameBoard = new GameBoard()
    val board = gameBoard.getBoard

    for(i <- GameConstants.PLAYER_ONE_FIRST_HOLE_INDEX until GameConstants.PLAYER_ONE_BASE_INDEX) {
      board(i) = 0
    }

    gameBoard.finishGame()

    assert(board(GameConstants.PLAYER_TWO_BASE_INDEX) == 36 && board(GameConstants.PLAYER_ONE_BASE_INDEX) == 0)
  }

  test("normalTurnsTest") {
    val gameBoard = new GameBoard()

    gameBoard.makeMove(1)

    assert(gameBoard.getBoard sameElements Array(6, 0, 7, 7, 7, 7, 1, 7, 6, 6, 6, 6, 6, 0))

    gameBoard.makeMove(7)

    assert(gameBoard.getBoard sameElements Array(7,0,7,7,7,7,1,0,7,7,7,7,7,1))
  }

  test("cloneTest") {
    val gameBoard = new GameBoard()
    val clone = gameBoard.clone()

    clone.getBoard(3) = 900

    assert(gameBoard.getBoard(3) != clone.getBoard(3))
  }

  test("invalidMoveTest") {
    val gameBoard = new GameBoard()
    gameBoard.getBoard(1) = 0

    assert(!gameBoard.isMoveValid(1))
    assert(!gameBoard.isMoveValid(7))

    gameBoard.makeMove(5)

    assert(!gameBoard.isMoveValid(4))
  }
}
