import L7.Board
import org.scalatest.FunSuite

class BoardTests extends FunSuite{
  var board = new Board
  board.prepareBoard()
  test("prepareboard"){
    assert(board.houses(0).toList == List(6, 6, 6, 6, 6, 6, 0))
    assert(board.houses(1).toList == List(6, 6, 6, 6, 6, 6, 0))
    assert(board.houses(0)(board.BaseIndex) == 0)
    assert(board.houses(1)(board.BaseIndex) == 0)
  }

  test("moveStones"){
    board.move_Stones(0, 0)
    assert(board.houses(0).toList == List(0, 7, 7, 7, 7, 7, 1))
    assert(board.houses(1).toList == List(6, 6, 6, 6, 6, 6, 0))
    assert(board.isExtraMove)
  }

  test("gamePossible") {
    assert(board.isGamePossible(0))
    assert(board.isGamePossible(1))
  }


  test("houseCapture"){
    board.prepareBoard()
    assert(board.houses(0).toList == List(6, 6, 6, 6, 6, 6, 0))
    assert(board.houses(1).toList == List(6, 6, 6, 6, 6, 6, 0))
    assert(board.houseCapture(0)( 1) == (-1, 0))
    board.move_Stones(0, 0)
    assert(board.houses(0).toList == List(0, 7, 7, 7, 7, 7, 1))
    assert(board.houseCapture(0)( 1) == (1, 6))
  }

  test("SeedsToBase"){
    assert(board.houses(0).toList == List(0, 7, 7, 7, 7, 7, 1))
    board.SeedsToBase(0)
    assert(board.houses(0).toList == List(0, 0, 0, 0, 0, 0, 36))
    //
    assert(board.houses(1).toList == List(6, 6, 6, 6, 6, 6, 0))
    board.SeedsToBase(1)
    assert(board.houses(1).toList == List(0, 0, 0, 0, 0, 0, 36))
  }
}



