import org.scalatest.flatspec.AnyFlatSpec

class Tests extends AnyFlatSpec{
  "Board class shouldPlayerRepeatTheMove method" should "return true when last stone was thrown to player's base" in {
    val board: Board = new Board
    board.takeFromHole(1, PlayerOne)
    assert(board.shouldPlayerRepeatTheMove)
  }

  "Board class shouldPlayerRepeatTheMove method" should "return false when last stone wasn't thrown to player's base" in {
    val board: Board = new Board
    board.takeFromHole(2, PlayerOne)
    assert(!board.shouldPlayerRepeatTheMove)
  }

  "playerOneAvailableHoles method" should "return correct list of available holes for player one" in {
    val board: Board = new Board
    board.takeFromHole(3, PlayerOne)
    assert(board.playerOneAvailableHoles == List(1, 2, 4, 5, 6))
  }

  "playerTwoAvailableHoles method" should "return correct list of available holes for player two" in {
    val board: Board = new Board
    board.takeFromHole(3, PlayerTwo)
    assert(board.playerTwoAvailableHoles == List(1, 2, 4, 5, 6))
  }

  "amountOfStonesInFirstPlayerBase method" should "return correct result" in {
    val board: Board = new Board
    assert(board.amountOfStonesInFirstPlayerBase == 0)
    board.takeFromHole(2, PlayerOne)
    assert(board.amountOfStonesInFirstPlayerBase == 1)
    board.takeFromHole(3, PlayerOne)
    assert(board.amountOfStonesInFirstPlayerBase == 2)
  }

  "amountOfStonesInSecondPlayerBase method" should "return correct result" in {
    val board: Board = new Board
    assert(board.amountOfStonesInSecondPlayerBase == 0)
    board.takeFromHole(2, PlayerTwo)
    assert(board.amountOfStonesInSecondPlayerBase == 1)
    board.takeFromHole(3, PlayerTwo)
    assert(board.amountOfStonesInSecondPlayerBase == 2)
  }

  "shouldGameBeContinued method" should "return true when both players contain stones in at least one hole" in {
    val board: Board = new Board
    assert(board.shouldGameBeContinued)
    board.takeFromHole(2, PlayerOne)
    assert(board.shouldGameBeContinued)
  }

  "selectHoleWithTheSmallestAmountOfStones method" should "return correct number" in {
    val board: Board = new Board
    board.takeFromHole(4, PlayerOne)
    board.takeFromHole(3, PlayerOne)
    board.takeFromHole(1, PlayerOne)
    assert(board.selectHoleWithTheSmallestAmountOFStones(PlayerOne) == 3)
  }
}
