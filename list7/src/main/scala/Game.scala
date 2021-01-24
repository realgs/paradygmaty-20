class Game() {
  var board = new Board
  var player1: Player = _
  var player2: Player = _

  def start(): Unit = {
    printInstruction()
    init()
    println("\nLet's play!")
    board.printBoard()
    makeMove(player1)
  }

  def init(): Unit = {
    printMenu()
    var choice = 0
    try {
      choice = scala.io.StdIn.readInt()
    } catch {
      case _: Exception => choice = 0
    }
    choice match {
      case 1 =>
        player1 = new Player(1)
        player2 = new Player(2)
      case 2 =>
        player1 = new Player(1)
        player2 = new SmartComputer(2)
      case 3 =>
        player1 = new Player(1)
        player2 = new RandomComputer(2)
      case 4 =>
        player1 = new SmartComputer(1)
        player2 = new SmartComputer(2)
      case _ =>
        println("Wrong input. Try again!\n")
        init()
    }
  }

  def makeMove(player: Player): Unit = {
    val choice = player.makeMove(board)
    val currentPlayer = player.number
    if (currentPlayer == 2) {
      if (choice >= 0 && choice < 6) tryAgain(player)
    }
    if (currentPlayer == 1) {
      if (choice >= 7 && choice < 13) tryAgain(player)
    }
    if (board.get(choice).isEmpty) tryAgain(player)
    var seedToDrop = board.get(choice).seedsNumber
    board.get(choice).seedsNumber = 0
    var currentPit = choice + 1
    while (seedToDrop > 0) {
      if (currentPit > 13) currentPit = 0
      if (board.get(currentPit).isInstanceOf[EndZone]) {
        if ((currentPlayer == 1 && currentPit == 13) || (currentPlayer == 2 && currentPit == 6)) {
          seedToDrop += 1
        }
      }
      seedToDrop -= 1
      if (seedToDrop == 0) {
        if ((currentPlayer == 1 && currentPit == 6) || (currentPlayer == 2 && currentPit == 13)) {
          board.get(currentPit).seedsNumber += 1
          if (checkBoard()) finalActions()
          board.printBoard()
          println("Your turn again")
          makeMove(player)
        }
        else if (board.get(currentPit).seedsNumber == 0) {
          stealSeeds(currentPlayer, currentPit)
        }
      }
      board.get(currentPit).seedsNumber += 1
      currentPit += 1
    }
    checkGameState(currentPlayer)

  }


  def tryAgain(player: Player): Unit = {
    println("Wrong input. Try again.")
    makeMove(player)
  }

  def stealSeeds(player: Int, position: Int): Unit = {
    val robbedPit = board.getOppositePit(position)
    addSeeds(player, robbedPit)
    //board.printBoard()
  }

  def addSeeds(player: Int, pitNumber: Int): Unit = {
    var stolenSeeds = board.get(pitNumber).seedsNumber
    board.get(pitNumber).seedsNumber = 0
    if (player == 1) board.get(6).seedsNumber += stolenSeeds
    else board.get(13).seedsNumber += stolenSeeds
  }

  def printInstruction(): Unit = {
    println("Welcome in Kalaha game")
    println("The game provides a Kalah board and a number of seeds.")
    println("Board has 6 small pits on each side and a big pit, called an end zone, at each end")
    println("Initially, each pit (except end zones) has 4 seeds.")
    println("The object of the game is to capture more seeds than one's opponent.\n")
    println("Fields numeration:")
    println("Player 2:              12 11 10 9  8  7         ")
    println("                      (4)(4)(4)(4)(4)(4)")
    println("Player2's end zone-|0|                  |0|-Player1's end zone")
    println("                      (4)(4)(4)(4)(4)(4)")
    println("Player 1:              0  1  2  3  4  5         ")

  }

  def printMenu(): Unit = {
    println("Menu")
    println("Options:")
    println("1. player vs player")
    println("2. player vs smart computer")
    println("3. player vs random computer")
    println("4. computer vs computer")
    print("Selected option: ")
  }

  def checkGameState(player: Int): Unit = {
    board.printBoard()
    if (checkBoard()) finalActions()
    else {
      if (player == 1) makeMove(player2)
      else makeMove(player1)
    }
  }

  def checkBoard(): Boolean = {
    var finish: Boolean = true
    for (i <- 0 to 5) {
      if (!board.get(i).isEmpty) finish = false
    }
    if (finish) return finish
    else
      finish = true
    for (i <- 7 to 12) {
      if (!board.get(i).isEmpty) finish = false
    }
    finish
  }

  def totalSeedsUp(): Unit = {
    for (i <- 0 to 5) {
      if (!board.get(i).isEmpty) addSeeds(1, i)
    }
    for (i <- 7 to 12) {
      if (!board.get(i).isEmpty) addSeeds(2, i)
    }
  }

  def finalActions(): Unit = {
    totalSeedsUp()
    printResults()
    ifEndOfTheGame()
  }

  def ifEndOfTheGame(): Unit = {
    println("Do you want play again? ")
    println("1. yes")
    println("2. no")
    print("Selected option: ")
    var choice = 3
    try {
      choice = scala.io.StdIn.readInt()
    } catch {
      case _: Exception => choice = 3
    }
    choice match {
      case 1 =>
        board = board.reset()
        start()
      case 2 =>
        println("End of the game")
        System.exit(0)
      case _ =>
        println("Wrong input. Try again\n")
        ifEndOfTheGame()
    }
  }

  def printResults(): Unit = {
    println("Final results")
    board.printBoard()
    if (board.get(6).seedsNumber == board.get(13).seedsNumber) println("It's a draw!")
    else if (board.get(6).seedsNumber > board.get(13).seedsNumber) println("Player 1 has won!")
    else println("Player 2 has won!")
  }


}
