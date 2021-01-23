import scala.util.control.Breaks._

class SmartComputer(server: ActorRef, playerNumber: Int) extends Actor {
  private val playerID: Int = playerNumber
  private val maxPlayer = playerID
  private val minPlayer = if (playerID == 0) 1 else 0

  server ! Connect(playerID)

  override def receive: Receive = onMessage(playerID)

  private def onMessage(playerID: Int): Receive = {
    case Move(gameBoard: GameBoard) =>
      println(s"Your turn Player $playerID: ")
      server ! MakeMove(getBestMove(gameBoard.copy()))

    case Disconnect() =>
      self ! PoisonPill
  }

  private def getBestMove(gameBoard: GameBoard): Int = {
    var bestMove = -1
    var bestScore = Integer.MIN_VALUE

    for (i <- 0 until 6) {
      if (gameBoard.checkMove(i)) {
        val gbCopy = gameBoard.copy()
        gbCopy.makeMove(i)

        val score = minMax(gbCopy, 5, maximizingPlayer = playerID == gbCopy.getCurrentPlayer, Integer.MIN_VALUE, Integer.MAX_VALUE)

        if (score > bestScore) {
          bestScore = score
          bestMove = i
        }
      }
    }

    println("Computer move: " + Console.RED + s"$bestMove")
    bestMove
  }

  private def minMax(gameBoard: GameBoard, depth: Int, maximizingPlayer: Boolean, a: Int, b: Int): Int = {
    var maxScore = Integer.MIN_VALUE
    var minScore = Integer.MAX_VALUE
    var score = 0

    var beta = b
    var alpha = a

    if (depth == 0) return gameBoard.getBaseScore(maxPlayer) - gameBoard.getBaseScore(minPlayer)
    if (gameBoard.checkGameStatus() != 2) return getUtility(gameBoard)

    if (maximizingPlayer) {
      breakable {
        for (i <- 0 until 6) {
          if (gameBoard.checkMove(i)) {
            val gbCopy = gameBoard.copy()
            gbCopy.makeMove(i)

            score = minMax(gbCopy, depth - 1, maximizingPlayer = false, alpha, beta)
            maxScore = Math.max(maxScore, score)
            alpha = Math.max(alpha, score)
            if (beta <= alpha)
              break()
          }
        }
      }
      maxScore
    }
    else {
      breakable {
        for (i <- 0 until 6) {
          if (gameBoard.checkMove(i)) {
            val gbCopy = gameBoard.copy()
            gbCopy.makeMove(i)

            score = minMax(gbCopy, depth - 1, maximizingPlayer = true, alpha, beta)
            minScore = Math.min(minScore, score)
            beta = Math.min(beta, score)
            if (beta <= alpha)
              break()

          }
        }
      }
      minScore
    }
  }

  private def getUtility(gameBoard: GameBoard): Int = {
    if (maxPlayer == 1) gameBoard.getFinalScore(1) - gameBoard.getFinalScore(0)
    else gameBoard.getFinalScore(0) - gameBoard.getFinalScore(1)
  }

}
