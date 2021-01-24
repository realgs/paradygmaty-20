class SmartComputer(num: Int) extends Player(num) {
  var start = 0
  var end = 5
  var endZone = 6
  if (num == 2) {
    start = 7
    end = 12
    endZone = 13
  }

  override def makeMove(board: Board): Int = {
    var choice = -1
    choice = bestPossibleMove(board)
    println(s"\nComputer's choice: $choice")
    choice
  }

  //fill empty pit
  def bestPossibleMove(board: Board): Int = {
    var bestMove = -1
    for (i <- start to end) {
      if (board.get(i).isEmpty) {
        if (i != start) {
          for (j <- start to i) {
            if ((i - j) == board.get(j).seedsNumber && !board.get(board.getOppositePit(j)).isEmpty) {
              if(!board.get(j).isEmpty) return j
            }
          }
        }
      }
    }

    //check if opponent have empty pit and free the opposite field
    var opponentStart = 7
    var opponentEnd = 12
    if (num == 2) {
      opponentStart = 0
      opponentEnd = 5
    }
    for (i <- opponentStart to opponentEnd) {
      if (board.get(i).isEmpty && !board.get(board.getOppositePit(i)).isEmpty) return board.getOppositePit(i)
    }

    //finish move on your endZone, to make move again
    for (i <- start to end) {
      if (!board.get(i).isEmpty) {
        if (board.get(i).seedsNumber == (endZone - i)) {
          if(!board.get(i).isEmpty) return i
        }
      }
    }

    //choose pit with the most seeds
    var maxSeeds = 0
    for (i <- start to end) {
      if (board.get(i).seedsNumber > maxSeeds) {
        maxSeeds = board.get(i).seedsNumber
        bestMove = i;
      }
    }

    return bestMove;
  }

}
