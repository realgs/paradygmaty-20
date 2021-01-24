class Logic(board: Board = new Board()) extends Simulator(board) {
  def makeMove(position: Int, player: Int): (Boolean, Boolean) = {
    val (b1, b2) = move(board, position - 1, player)
    if (b1) println("Player" + (player + 1) + " move from position: " + position)
    (b1, b2)
  }

  def getBoard: Board = board

  def getPlayer1Points: Int = board.getMancalas(0).get()

  def getPlayer2Points: Int = board.getMancalas(1).get()

  def print(): Unit = board.print()
}
