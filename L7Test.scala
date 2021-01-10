import org.scalatest.FunSuite
import Kalaha._

class L7Test extends FunSuite {

  test("Basic test for moves method: ") {
    val board: Board = new Board(4)
    assert(board.makeMove(0, 2) == 0)
    assert(board.getStonesNumber(0) == 4)
    assert(board.makeMove(0, 1) == -1)
    assert(board.getStonesNumber(1) == 5)
    assert(board.getStonesNumber(2) == 5)
    assert(board.getStonesNumber(3) == 5)
    assert(board.getStonesNumber(4) == 5)
    assert(board.makeMove(9, 2) == 1)
    assert(board.getStonesNumber(9) == 0)
    assert(board.getStonesNumber(10) == 5)
    assert(board.getStonesNumber(11) == 5)
    assert(board.getStonesNumber(12) == 5)
    assert(board.getStonesNumber(13) == 1)
  }

  test("Test for moves for player 1 when he have to omit main base of player 2: ") {
    val board: Board = new Board(4)
    board.board(0) = 19
    /* We have already that situation:
    0  4 4 4 4 4 4 0
      19 4 4 4 4 4

      And when we chose hole number 0, we get:
    0 5 5 5 5 5 5 2
      1 6 6 6 6 6

      And we should have a next moves, so the method return 1
     */
    assert(board.makeMove(0, 1) == 1)
    assert(board.getStonesNumber(0) == 1)
    assert(board.getStonesNumber(1) == 6)
    assert(board.getStonesNumber(2) == 6)
    assert(board.getStonesNumber(3) == 6)
    assert(board.getStonesNumber(4) == 6)
    assert(board.getStonesNumber(5) == 6)
    assert(board.getStonesNumber(6) == 2)
    assert(board.getStonesNumber(7) == 5)
    assert(board.getStonesNumber(8) == 5)
    assert(board.getStonesNumber(9) == 5)
    assert(board.getStonesNumber(10) == 5)
    assert(board.getStonesNumber(11) == 5)
    assert(board.getStonesNumber(12) == 5)
    assert(board.getStonesNumber(13) == 0)
  }

  test("Test for computer player moves, if it choose the most appropriate move: ") {
    /* We have already that situation:
    0 4 4 4 4 4 4 0
      4 4 4 4 4 4

    He should chose number 2 if it is player number 1, and hole: 9 if it is player 2, because it gives one more move*/
    val board: Board = new Board(4)
    val tree = new DecisionTree(board, 1)
    tree.createFirstLevel()
    tree.createTree()
    val chosenHole = tree.findBestDecision(tree.createChoiceQueue(tree.getRoot()))
    assert(chosenHole == 2)
    //For computera as player 2
    val board2: Board = new Board(4)
    val tree2 = new DecisionTree(board2, 2)
    tree2.createFirstLevel()
    tree2.createTree()
    val chosenHole2 = tree2.findBestDecision(tree2.createChoiceQueue(tree2.getRoot()))
    assert(chosenHole2 == 9)
  }

  test("Test2 for computer player moves, if it choose the most appropriate move: ") {
    /* We have already that situation:
        0 1 0 2 0 4 3 0
          4 4 3 3 4 4

    He should chose number 12 if it is player number 2, and hole: 3 if it is player 1, because it gives one more move*/
    val board: Board = new Board(4)
    board.board(2) = 3
    board.board(3) = 3
    board.board(7) = 3
    board.board(9) = 0
    board.board(10) = 2
    board.board(11) = 0
    board.board(12) = 1
    val tree = new DecisionTree(board, 1)
    tree.createFirstLevel()
    tree.createTree()
    val chosenHole = tree.findBestDecision(tree.createChoiceQueue(tree.getRoot()))
    assert(chosenHole == 3)
    //For computera as player 2
    val board2: Board = new Board(4)
    board.board(2) = 3
    board2.board(3) = 3
    board.board(7) = 3
    board2.board(9) = 0
    board2.board(10) = 2
    board2.board(11) = 0
    board2.board(12) = 1
    val tree2 = new DecisionTree(board2, 2)
    tree2.createFirstLevel()
    tree2.createTree()
    val chosenHole2 = tree2.findBestDecision(tree2.createChoiceQueue(tree2.getRoot()))
    assert(chosenHole2 == 12)
  }
}
