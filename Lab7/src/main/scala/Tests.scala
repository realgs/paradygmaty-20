object Tests {

  def test(): Unit = {

    val board = new Board(6)

    board.makeMove(2)
    board.makeMove(6)
    board.makeMove(1)
    board.makeMove(2)
    board.makeMove(5)
    board.makeMove(3)
    board.makeMove(6)
    board.makeMove(4)
    board.makeMove(6)
    board.makeMove(2)
    board.makeMove(6)
    board.makeMove(5)
    board.makeMove(6)
    board.makeMove(3)

    assert(board.getScores == (23, 4))

    board.makeMove(2)
    board.makeMove(6)
    board.makeMove(1)
    board.makeMove(6)
    board.makeMove(5)
    board.makeMove(6)
    board.makeMove(4)

    assert(board.getScores == (29, 5))

    board.makeMove(6)
    board.makeMove(6)
    board.makeMove(4)
    board.makeMove(6)
    board.makeMove(5)
    board.makeMove(6)
    board.makeMove(3)
    board.makeMove(4)
    board.makeMove(4)

    assert(board.getScores == (45, 11))

    board.makeMove(2)
    board.makeMove(5)
    board.makeMove(4)
    board.makeMove(6)
    board.makeMove(5)
    board.makeMove(6)
    board.makeMove(2)
    board.makeMove(6)
    board.makeMove(3)

    assert(board.isGameOver)
    assert(board.getScores == (56, 16))
  }
}
