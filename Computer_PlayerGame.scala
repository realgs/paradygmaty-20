package L7

object Computer_PlayerGame {
  def run(initialBoard: Board, isPrintingInfo: Boolean, isHardVersion: Boolean): Unit = {
    if (isPrintingInfo) {
      System.out.println("The game has started")
      initialBoard.printBoard()
    }
    var numberOfLevels = 2
    if (isHardVersion) {
      numberOfLevels = 4
    }

    var player1Turn = true
    var isGameLasting = true
    var houseIndex = -1
    while (isGameLasting) {
      if (player1Turn == true) {
        houseIndex = ComputerDecisionTree.makeDecision(initialBoard, true, numberOfLevels)
        var result = (false, false)

        System.out.println("chosen house index: " + houseIndex)
        result = initialBoard.makeMove(1, houseIndex)

        if (isPrintingInfo) {
          System.out.println("\nBoard after player 1 move:")
          initialBoard.printBoard()
        }
        if (result._2 == false) player1Turn = false
      }
      else {
        if (initialBoard.isEndOfGame(false) == true) {
          isGameLasting = false
        }
        if (isGameLasting == true) {
          var isPossible = false
          while (isPossible == false) {
            System.out.println("Write house input: (from 7 to 12): ")
            val input: Int = scala.io.StdIn.readInt()
            val answer = initialBoard.makeMove(2, input)
            isPossible = answer._1
            if (isPossible == true && answer._2 == false) {
              System.out.println("User move has been done")
              initialBoard.printBoard()
              player1Turn = true
            }
          }
        }
      }

      if (houseIndex == -1 || isGameLasting == false) {
        if (player1Turn == true) {
          initialBoard.getAllSelfStones(2)
        }
        else {
          initialBoard.getAllSelfStones(1)
        }
        initialBoard.printEndResults()
        initialBoard.printBoard()
        isGameLasting = false
      }
    }
  }
}

