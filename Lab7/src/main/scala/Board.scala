class Board {

  private val startStonesNumber = 6
  private val board: Array[Int] = createBoard()
  var ifPlayer1Move = true
  var ifOneAgainMove = false
  var ifGameIsOver = false

  def makeMove(fieldNumber: Int): Boolean = {

    if(fieldNumber < 1 || fieldNumber > 6 || board(fieldNumber + (if(ifPlayer1Move) -1 else 6)) == 0) false
    else {

      var index = fieldNumber + (if(ifPlayer1Move) -1 else 6)
      var stones = board(index)

      board(index) = 0

      while (stones > 0) {

        index = (index + 1) % 14

        if (index != (if(ifPlayer1Move) 13 else 6)) {

          board(index) = board(index) + 1
          stones -= 1
        }
      }

      if(index == (if(ifPlayer1Move) 6 else 13)) ifOneAgainMove = true
      else if(board(index) == 1 && index >= (if(ifPlayer1Move) 0 else 7) && index <= (if(ifPlayer1Move) 5 else 12)) {

        board(if(ifPlayer1Move) 6 else 13) += board(12 - index) + 1
        board(12 - index) = 0
        board(index) = 0
        ifPlayer1Move = !ifPlayer1Move

      } else ifPlayer1Move = !ifPlayer1Move

      checkIfEndOfGame()

      true
    }
  }

  private def createBoard(): Array[Int] = {

    val board = Array.fill(14)(startStonesNumber)
    board(6) = 0
    board(13) = 0

    board
  }

  def print(): Unit = {

    printf("%n    | %2d | %2d | %2d | %2d | %2d | %2d |%n", board(12), board(11), board(10), board(9), board(8), board(7))
    printf(" %2d |-----------------------------| %2d", board(13), board(6))
    printf("%n    | %2d | %2d | %2d | %2d | %2d | %2d |%n", board(0), board(1), board(2), board(3), board(4), board(5))
  }

  def ifCorrectFieldNumber(fieldNumber: Int): Boolean =
    fieldNumber >= 1 && fieldNumber <= 6 && board(fieldNumber + (if(ifPlayer1Move) -1 else + 6)) != 0

  def printResult(): Unit = {

    val player1Score = board(6)
    val player2Score = board(13)

    printf("%nPlayer 1. score: %2d", player1Score)
    printf("%nPlayer 2. score: %2d", player2Score)
    if(player1Score == player2Score) println("\nDraw!")
    else printf("%n%nPlayer %d. won!%n", if(player1Score > player2Score) 1 else 2)
  }

  def checkIfEndOfGame(): Unit = {

    ifGameIsOver = true

    for (i <- if(ifPlayer1Move) 7 to 12 else 0 to 6)
        ifGameIsOver = ifGameIsOver && board(i) == 0
  }
}
