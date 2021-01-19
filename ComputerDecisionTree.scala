package L7

object ComputerDecisionTree {
  case class Move(houseIndex: Int, wasMoveValid: Boolean, boardAfterMove: Board, firstMoveHouseIdx: Int, isEndOfGame: Boolean)

  private def generateFirstPossibleMoves(currentBoard: Board, isPlayer1: Boolean): List[Move] = {
    var possibleMoves: List[Move] = Nil
    var playerNumber = 1
    var firstHouseIndex = 0
    if (isPlayer1 == false) {
      playerNumber = 2
      firstHouseIndex = 7
    }
    for (i <- 0 until 6) {
      val currentHouseIndex = firstHouseIndex + i
      val boardAfterCurrentMove = currentBoard.cloneBoard()
      val (wasValid, isEndOfGame) = boardAfterCurrentMove.updatePlayer(playerNumber, currentHouseIndex)
      if (wasValid == true)
        possibleMoves = Move(currentHouseIndex, wasValid, boardAfterCurrentMove, currentHouseIndex, isEndOfGame) :: possibleMoves
    }
    possibleMoves
  }

  def generateFurtherPossibleMoves(earlierMoves: List[Move], isPlayer1: Boolean): List[List[Move]] = {
    var possibleMoves: List[Move] = Nil
    var listOfPossibleMoves: List[List[Move]] = Nil // every element is a list of possible moves after a move made before
    var playerNumber = 1
    var firstHouseIndex = 0
    if (isPlayer1 == false) {
      playerNumber = 2
      firstHouseIndex = 7
    }
    for(i <- 0 until earlierMoves.length) {
      val oneMoveEarlier: Move = earlierMoves(i)
      if (oneMoveEarlier.wasMoveValid) {
        for (j <- 0 until 6) {
          val currentHouseIndex = firstHouseIndex + j
          val boardAfterCurrentMove = oneMoveEarlier.boardAfterMove.cloneBoard()
          val (wasValid, isEndOfGame) = boardAfterCurrentMove.updatePlayer(playerNumber, currentHouseIndex)
          possibleMoves = Move(currentHouseIndex, wasValid, boardAfterCurrentMove, oneMoveEarlier.firstMoveHouseIdx, isEndOfGame) :: possibleMoves
        }
        listOfPossibleMoves = possibleMoves :: listOfPossibleMoves
        possibleMoves = Nil
      }
    }
    listOfPossibleMoves.reverse
  }

  private def numberOfWinningIndex(possibleMoves: List[Move], isPlayer1: Boolean): Int = {
    for (i <- 0 until possibleMoves.length) {
      val move: Move = possibleMoves(i)
      if (move.boardAfterMove.isEndOfGame() && move.boardAfterMove.getAdvantage(isPlayer1) > 0 ) {
        return move.firstMoveHouseIdx
      }
    }
    -1
  }

  private def innerSearchBestMove(possibleMoves: List[Move], selfEndZone: Int, enemyEndZone: Int): (Int, Move) = { // returns -1 if any move can't be done
    var advantage = Integer.MIN_VALUE
    var bestMove: Move = null
    var bestHouseIdx = -1
    for (i <- 0 until possibleMoves.length) {
      val currentMove = possibleMoves(i)
      if (currentMove != null && currentMove.wasMoveValid) {
        val currentAdvantage = currentMove.boardAfterMove.getBoard() (selfEndZone) - currentMove.boardAfterMove.getBoard() (enemyEndZone)
        if (currentMove.isEndOfGame && currentAdvantage > 0) { // if game will end and we win -> it is the best choice
          return (currentMove.firstMoveHouseIdx, currentMove)
        }
        if (currentAdvantage > advantage) { //searching for index with biggest advantage after particular move
          advantage = currentAdvantage
          bestHouseIdx = currentMove.firstMoveHouseIdx
          bestMove = currentMove
        }
      }
    }
    (bestHouseIdx, bestMove)
  }

  private def searchBestMoveIdx(possibleMoves: List[Move], isBestForPlayer1: Boolean): Int = {
    var selfEndZone = 6
    var enemyEndZone = 13
    if (isBestForPlayer1 == false) {
      selfEndZone = 13
      enemyEndZone = 6
    }
    innerSearchBestMove(possibleMoves, selfEndZone, enemyEndZone)._1
  }

  private def searchBestMoveForEnemy(possibleMoves: List[Move], isDecisionTreeForPlayer1: Boolean): Move = {
    var enemyEndZone = 6  // indexes in enemy's point of view (for enemy his enemy is player1)
    var selfEndZone = 13
    if (!isDecisionTreeForPlayer1) {
      enemyEndZone = 13
      selfEndZone = 6
    }
    innerSearchBestMove(possibleMoves, selfEndZone, enemyEndZone)._2
  }

  private def toOneDimensionList(listOfPossibleMoves: List[List[Move]], isEnemyPossibleMoves: Boolean, isDecisionTreeForPlayer1: Boolean): (Int, List[Move]) = {
    var oneDimensionList: List[Move] = Nil
    if (isEnemyPossibleMoves) {
      for (i <- 0 until listOfPossibleMoves.length) {
        if (!listOfPossibleMoves(i).isEmpty) {
          val move: Move = searchBestMoveForEnemy(listOfPossibleMoves (i), isDecisionTreeForPlayer1)
          if (move == null) {
            val boardAfterEarlierMove: Board = listOfPossibleMoves(i)(0).boardAfterMove
            if (boardAfterEarlierMove.getAdvantage(isDecisionTreeForPlayer1) > 0 )
              return (listOfPossibleMoves(i)(0).firstMoveHouseIdx, Nil)
          }
          else oneDimensionList = move :: oneDimensionList
        }
      }
    }
    else {
      for (i <- 0 until listOfPossibleMoves.length) {
        if (!listOfPossibleMoves(i).isEmpty) {
          val winningIndex = numberOfWinningIndex(listOfPossibleMoves(i), isDecisionTreeForPlayer1)
          if (winningIndex != -1) return (winningIndex, Nil)
          for (j <- 0 until listOfPossibleMoves(i).length) {
            oneDimensionList = listOfPossibleMoves(i)(j) :: oneDimensionList
          }
        }
      }
    }
    (-1, oneDimensionList.reverse)
  }

  def printMovesLevel(string: String, moves: List[Move]) : Unit = {
    System.out.println(string)
    for (i <- 0 until moves.length) {
      System.out.print(moves(i))
      System.out.print("\t")
    }
    System.out.print("\n")
  }

  def checkHowManyMovesCanMakeAtBeginning(listOfMoves: List[Move]): Int = {
    var counter = 0
    for (i <- 0 until listOfMoves.length) {
      if (listOfMoves(i).wasMoveValid == true)
        counter = counter + 1
    }
    counter
  }

  def makeDecision(actualBoard: Board, isPlayer1: Boolean, numberOfLevels: Int): Int = {  // returns -1 if move can't be done
    val playerFirstPossibleMoves = generateFirstPossibleMoves(actualBoard.cloneBoard(), isPlayer1)   // player moves
    if (playerFirstPossibleMoves.isEmpty) return -1
    if (playerFirstPossibleMoves.length == 1) return playerFirstPossibleMoves(0).firstMoveHouseIdx

    var numberOfReps = numberOfLevels
    var actualListOfMoves = playerFirstPossibleMoves
    var winningIndex: Int = -1
    var oneDimensionAnswers: List[Move] = Nil
    while(numberOfReps > 0) {
      val enemyAnswers = generateFurtherPossibleMoves(actualListOfMoves, !isPlayer1)  // enemy answers
      val enemyResult = toOneDimensionList(enemyAnswers, true, isPlayer1)
      winningIndex = enemyResult._1
      oneDimensionAnswers = enemyResult._2
      if (winningIndex != -1) return winningIndex

      val playerAnswers = generateFurtherPossibleMoves(oneDimensionAnswers, isPlayer1)  // player answers
      val playerResults = toOneDimensionList(playerAnswers, false, isPlayer1)
      winningIndex = playerResults._1
      oneDimensionAnswers = playerResults._2
      if (winningIndex != -1) return winningIndex
      numberOfReps = numberOfReps - 1
      actualListOfMoves = oneDimensionAnswers
    }
    searchBestMoveIdx(actualListOfMoves, isPlayer1)
  }
}

