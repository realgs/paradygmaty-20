object Main extends App {
  val gameBoard = new GameBoard()

  gameBoard.printBoard()
  gameBoard.makeMove(1)

  gameBoard.printBoard()

}
