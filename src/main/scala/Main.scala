object Main extends App {
  val gameBoard = new GameBoard()

  gameBoard.printBoard()

  gameBoard.makeMove(1)

  gameBoard.printBoard()

  gameBoard.makeMove(7)

  gameBoard.printBoard(Turn.SecondPlayer)
  gameBoard.printBoard()
}
