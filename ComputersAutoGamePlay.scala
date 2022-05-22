package L7

object ComputersAutoGamePlay {
  def runSmart(initialBoard: Board, isPrintingInfo: Boolean, isHardVersion: Boolean): Unit = {
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
    while (isGameLasting) {
      val houseIndex = ComputerDecisionTree.makeDecision(initialBoard, player1Turn, numberOfLevels)
      if (houseIndex == -1) {
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
      else {
        var result = (false, false)
        if (player1Turn) result = initialBoard.makeMove(1, houseIndex)
        else result= initialBoard.makeMove(2, houseIndex)
        if (isPrintingInfo) {
          if (player1Turn == true) System.out.println("\nBoard after player 1 move:")
          else System.out.println("\nBoard after player 2 move:")
          initialBoard.printBoard()
        }
        if (result._2 == false)
          player1Turn = !player1Turn
      }
    }
  }

  def runRandom(initialBoard: Board, isPrintingInfo: Boolean): Unit = {
    if (isPrintingInfo) {
      System.out.println("The game has started")
      initialBoard.printBoard()
    }
    var player1Turn = true
    var isGameLasting = true
    while (isGameLasting) {
      val houseIndex = initialBoard.getRandomValidMove(player1Turn)
      if (houseIndex == -1) {
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
      else {
        var result = (false, false)
        if (player1Turn) result = initialBoard.makeMove(1, houseIndex)
        else result= initialBoard.makeMove(2, houseIndex)
        if (isPrintingInfo) {
          if (player1Turn == true) System.out.println("\nBoard after player 1 move:")
          else System.out.println("\nBoard after player 2 move:")
          initialBoard.printBoard()
        }
        if (result._2 == false)
          player1Turn = !player1Turn
      }
    }
  }
}

