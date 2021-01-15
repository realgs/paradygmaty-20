import org.scalatest.FunSuite

class BoardTests extends FunSuite {

  test("takeOppositeStonesTest") {
    val gameBoard = new GameBoard()
    val board = gameBoard.getBoard

    board(1) = 1
    board(2) = 0
    board(10) = 20

    gameBoard.makeMove(1)

    assert(board(GameBoard.PLAYER_ONE_BASE_INDEX) == 21)
  }
}
