package Kalaha

class Board(var stonesInHole: Int) {

  val mainBase1 = 6
  val mainBase2 = 13

  //Creating a start board with appropriate number in each hole
  var board: Array[Int] = Array.fill(14)(stonesInHole)
  //Setting the number of stones in main bases on 0
  board(mainBase1) = 0
  board(mainBase2) = 0

  //Getting number of stones from selected hole
  def getStonesNumber(holeNumber: Int): Int = {
    require(holeNumber > -1 & holeNumber < 14, "Entered wrong hole number")
    board(holeNumber)
  }

  //Getting number of stones from main base of selected player
  def getScore(playerNumber: Int): Int = {
    require(playerNumber == 1 || playerNumber == 2, "Selected wrong player number")
    if(playerNumber == 1) getStonesNumber(mainBase1)
    else getStonesNumber(mainBase2)
  }

  //Getting main base of selected player
  def getMainBaseIndex(playerNumber: Int): Int = {
    require(playerNumber == 1 || playerNumber == 2, "Selected wrong player number")
    if(playerNumber == 1) mainBase1
    else mainBase2
  }

  //Setting score on appropriate main base
  def setScoreMainBase(playerNumber: Int, additionalStones: Int): Unit = {
    require(playerNumber == 1 || playerNumber == 2, "Selected wrong player number")
    board(getMainBaseIndex(playerNumber)) += additionalStones
  }

  //Checking if the hole is empty
  def isEmptyHole(holeNumber: Int): Boolean = {
    require(holeNumber > -1 & holeNumber < 14, "Entered wrong hole number")
    board(holeNumber) < 1
  }

  //Checking if the base is belong to selected player
  def isBelongPlayer(holeNumber: Int, playerNumber: Int): Boolean = {
    require(holeNumber > -1 & holeNumber < 14, "Entered wrong hole number")
    require(playerNumber == 1 || playerNumber == 2, "Selected wrong player number")
    if(playerNumber == 1) holeNumber < 6
    else holeNumber > 6 & holeNumber < 13
  }

  //Checking if the hole is simple base
  def isBase(holeNumber: Int): Boolean = {
    require(holeNumber > -1 & holeNumber < 14, "Entered wrong hole number")
    holeNumber != mainBase1 & holeNumber != mainBase2
  }

  //Checking if the hole is main base
  def isMainBase(holeNumber: Int): Boolean = {
    require(holeNumber > -1 & holeNumber < 14, "Entered wrong hole number")
    holeNumber == mainBase1 || holeNumber == mainBase2
  }

  //Sum of all bases for selected player
  def sumAllHoles(playerNumber: Int): Int = {
    require(playerNumber == 1 || playerNumber == 2, "Selected wrong player number")
    val rangeFrom = if(playerNumber == 1) 0 else 7
    val rangeUntil = if(playerNumber == 1) 5 else 12
    var sum = 0
    for(x <- rangeFrom to rangeUntil) {
      sum += getStonesNumber(x)
    }
    sum
  }

  //Sum of all bases for players
  def sumAllHolesForPlayers: (Int, Int) = (sumAllHoles(1), sumAllHoles(2))

  //Sum all others stone when one player have all holes empty
  def result: (Int, Int) = {
    if(ifEnd(1)) (getStonesNumber(mainBase1), getStonesNumber(mainBase2) + sumAllHoles(2))
    else (getStonesNumber(mainBase1) + sumAllHoles(1), getStonesNumber(mainBase2))
  }

  //Checking if the game should be ended, so all bases for selected player are empty
  def ifEnd(playerNumber: Int): Boolean = {
    sumAllHoles(playerNumber) == 0
  }

  //Cloning the current board
  override def clone(): Board = {
    val newBoard:Board = new Board(0)
    for(x <- 0 to board.size-1)
      newBoard.board(x) = board(x)
    newBoard
  }

  //Making move from selected hole by concrete player and returning if the player has next move
  //Return 0 if the data are incorrect - let server to wait the end of turn of player(30s)
  //Return 1 if the player threw last stone to own main base
  //Return -1 if the next turn is for another player
  def makeMove(selectedHole: Int, playerNumber: Int): Int = {
    require(playerNumber == 1 || playerNumber == 2, "Selected wrong player number")
    require(selectedHole > -1 & selectedHole < 14, "Entered wrong hole number")
    //If the selected hole doesn't belong to current player or is the own main base return 0
    if(playerNumber == 1 & selectedHole > 5) {
      return 0
    }
    if(playerNumber == 2 & selectedHole < 7 || selectedHole == 13) {
      return 0
    }
    //If selected hole is empty return 0
    if(isEmptyHole(selectedHole)) {
      return 0
    }

    var howManyStonesToThrow = getStonesNumber(selectedHole)
    var currentHole = selectedHole
    val opponent = if(playerNumber == 1) 2 else 1

    //Setting stones number of selected hole on 0
    board(selectedHole) = 0

    while(howManyStonesToThrow > 0) {
      //If the next base is the main base of opponent - omit this base
      if(currentHole+1 == getMainBaseIndex(opponent)) {
        if(opponent == 2) currentHole = -1
        else currentHole += 1
      } else {
        currentHole += 1
        board(currentHole) += 1
        howManyStonesToThrow -= 1
        if(currentHole == getMainBaseIndex(2)) {
          if(howManyStonesToThrow > 0) currentHole = -1
          else currentHole = 13
        }
      }
    }

    //If the last stone was thrown to own main base - next move
    if(currentHole == getMainBaseIndex(playerNumber)) {
      //println("The last stone was thrown to own main base - next move.")
      return 1
    }
    //If the last stone was thrown to empty hole - collect stone from opponent
    else if(getStonesNumber(currentHole) == 1 & isBelongPlayer(currentHole, playerNumber)) {
      //println("The last stone was thrown to empty hole.")
      //Collecting all stones from opponent hole
      val opponentHoleIndex = 12 - currentHole
      val collectedStones = getStonesNumber(opponentHoleIndex)
      board(opponentHoleIndex) = 0
      board(currentHole) = 0
      board(getMainBaseIndex(playerNumber)) += 1 + collectedStones
    }
    -1
  }

  //Returning number of player whose move is after the current one
  //ifEnable: 1 if next move belong to that same player, -1 if opposite, 0 if there was incorrect data while moving
  //Return: 1 or 2 (players) or -1 if the move belong to that same person, but the time won't be start once again, but only continue
  def whoseNextMove(ifEnable: Int, playerNumber: Int): Int = {
    val opponent = if(playerNumber == 1) 2 else 1
    if(ifEnable == 1) playerNumber
    else if(ifEnable == -1) opponent
    else -1
  }

  //Printing the board
  def printBoard: Unit = {
    print(getScore(2) + "\t\t")
    for(x <- 7 to 12) {
      print(getStonesNumber(19 - x) + "\t\t")
    }
    print(getScore(1))
    print("\n\t\t")
    for(x <- 0 to 5) {
      print(getStonesNumber(x) + "\t\t")
    }
    print("\n\n")
  }

  //Printing the results
  def printResults: Unit = {
    var (result1, result2) = (getStonesNumber(mainBase1), getStonesNumber(mainBase2))
    result1 += sumAllHoles(1)
    result2 += sumAllHoles(2)
    println("Player 1: " + result1)
    println("Player 2: " + result2)
    if(result1 > result2) println("Player 1 won.")
    else if (result1 == result2) println("There is a draw.")
    else println("Player 2 won.")
  }

}
