class Board {
  var board: Array[Pit] = new Array(14)
  for (i <- board.indices) {
    if (i == 6 || i == 13) board(i) = new EndZone(i, 0)
    else board(i) = new Pit(i, 4)
  }

  def get(i: Int): Pit = {
    return board(i)
  }

  def printBoard() {
    print("\n    ")
    for (i <- 12 to 7 by -1) {
      print(board(i))
    }
    println()
    print(board(13))
    print("                   ")
    print(board(6))
    println()
    print("    ")
    for (i <- 0 to 5) {
      print(board(i))
    }
    print("\n\n")
  }

  def reset(): Board = {
    return new Board()
  }

  def getClone(): Board = {
    var clonedBoard = new Board()
    clonedBoard.board = this.board.clone()
    return clonedBoard
  }

  def getOppositePit(position: Int): Int = {
    var oppositePos = -1
    position match {
      case 0 => oppositePos = 12
      case 1 => oppositePos = 11
      case 2 => oppositePos = 10
      case 3 => oppositePos = 9
      case 4 => oppositePos = 8
      case 5 => oppositePos = 7
      case 7 => oppositePos = 5
      case 8 => oppositePos = 4
      case 9 => oppositePos = 3
      case 10 => oppositePos = 2
      case 11 => oppositePos = 1
      case 12 => oppositePos = 0
      case _ => throw new Exception("Error")
    }
    oppositePos
  }

}
