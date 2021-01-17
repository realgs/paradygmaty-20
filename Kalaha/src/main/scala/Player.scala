abstract class Player(val name: String, val number: Int) {
  def play(gameStatus: Game): Int
}

class Human(name: String, number: Int) extends Player(name, number) {
  override def play(gameStatus: Game): Int = {
    scala.io.StdIn.readInt()
  }
}

object Human {
  def apply(name: String, number: Int): Human = new Human(name, number)
}

class AI(number: Int, depth: Int) extends Player(name = s"AI-$number", number) {

  override def play(gameStatus: Game): Int = {
    val bestMove = minimax(gameStatus,depth,Integer.MIN_VALUE,Integer.MAX_VALUE,gameStatus.getTurn % 2 == 1,-1)._2
    println(bestMove)
    bestMove
  }

  //Using min-max algorithm with alpha beta pruning
  def minimax(gamePosition: Game, depth: Int, alpha: Int, beta: Int, maximizingPlayer: Boolean, previousMove: Int): (Int,Int) = {
    if (depth == 0 || gamePosition.isGameOver) {
      if(gamePosition.emptySide()) gamePosition.gatherPebbles()
      (gamePosition.getBoard(6) - gamePosition.getBoard(13),previousMove)
    }

    else if (maximizingPlayer) {
      var newAlpha = alpha
      var optimalMove = -1
      gamePosition.validMoves().foreach(move => {
        val child = gamePosition.clone()
        child.move(move)
        val tmpPair = minimax(child,depth - 1,newAlpha,beta,child.getTurn % 2 == 1,move)
        if(tmpPair._1 > newAlpha) {
          newAlpha = tmpPair._1
          optimalMove = move
        }
        if(newAlpha >= beta) return (beta,move)
      })
      (newAlpha,optimalMove)
    }
    else {
      var newBeta = beta
      var optimalMove = -1
      gamePosition.validMoves().foreach(move => {
        val child = gamePosition.clone()
        child.move(move)
        val tmpPair = minimax(child,depth - 1,alpha,newBeta,child.getTurn % 2 == 1,move)
        if(tmpPair._1 < newBeta) {
          newBeta = tmpPair._1
          optimalMove = move
        }
        if(newBeta <= alpha) return (alpha,move)
      })
      (newBeta,optimalMove)
    }
  }

}

object AI {
  def apply(number: Int, depth: Int = 5): AI = new AI(number, depth)
}