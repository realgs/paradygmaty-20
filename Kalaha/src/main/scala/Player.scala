abstract class Player(val name: String, val number: Int) {
  def play(gameStatus: Game): Int
}

class Human(name: String, number: Int) extends Player(name, number) {
  override def play(gameStatus: Game): Int = {
    scala.io.StdIn.readLine().toInt
  }
}

object Human {
  def apply(name: String, number: Int): Human = new Human(name, number)
}

class AI(number: Int, depth: Int) extends Player(name = s"AI-$number", number) {

  override def play(gameStatus: Game): Int = {
    val bestMove = chooseAMove(gameStatus, number % 2 == 1)
    println(bestMove)
    bestMove
  }


  def chooseAMove(gameStatus: Game, maximizingPlayer: Boolean): Int = {
    var bestMove = -1
    var moveValue = 0
    var currentBestMoveValue = if (maximizingPlayer) Integer.MIN_VALUE else Integer.MAX_VALUE
    val validMoves = gameStatus.validMoves()
    for (i <- validMoves.indices) {
      val child = gameStatus.clone()
      child.move(validMoves(i))
      moveValue = minimax(child, depth,Integer.MIN_VALUE,Integer.MAX_VALUE,maximizingPlayer)
      if (maximizingPlayer && moveValue >= currentBestMoveValue || !maximizingPlayer && moveValue <= currentBestMoveValue) {
        currentBestMoveValue = moveValue
        bestMove = validMoves(i)
      }
    }
    bestMove
  }

  //Using min-max algorithm with alpha beta pruning
  def minimax(gamePosition: Game, depth: Int, alpha: Int, beta: Int, maximizingPlayer: Boolean): Int = {
    if (depth == 0 || gamePosition.isGameOver) gamePosition.getBoard(6) - gamePosition.getBoard(13)

    else if (maximizingPlayer) {
      var newAlpha = alpha
      gamePosition.validMoves().foreach(elem => {
        val child = gamePosition.clone()
        child.move(elem)
        newAlpha = math.max(alpha, minimax(child, depth - 1, newAlpha, beta, child.getTurn % 2 == 1))
        if (newAlpha >= beta) return beta
      })
      newAlpha
    }
    else {
      var newBeta = beta
      gamePosition.validMoves().foreach(elem => {
        val child = gamePosition.clone()
        child.move(elem)
        newBeta = math.min(beta, minimax(child, depth - 1, alpha, newBeta, child.getTurn % 2 == 1))
        if (newBeta <= alpha) return alpha
      })
      newBeta
    }
  }

}

object AI {
  def apply(number: Int, depth: Int = 5): AI = new AI(number, depth)
}