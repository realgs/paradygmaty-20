import akka.actor.{Actor, ActorRef, PoisonPill}

import scala.util.Random

class Server extends Actor {
  private val players = List[(Int, ActorRef)]()
  private val gameBoard: GameBoard = new GameBoard
  private var currentPlayer: Int = drawRound()

  override def receive: Receive = onMessage(players)

  private def drawRound(): Int = {
    Random.between(0, 1)
  }

  private def oppositePlayer(): Unit = {
    if (currentPlayer == 1)
      currentPlayer = 0
    else currentPlayer = 1
  }

  private def endGame() = {
    context.system.terminate()
  }

  private def onMessage(players: List[(Int, ActorRef)]): Receive = {
    case Connect(playerID: Int) =>
      context.become(onMessage((playerID, sender) :: players))
      context.watch(sender)
      if (currentPlayer == playerID) {
        gameBoard.printBoard()
        sender() ! move(gameBoard)
      }

    case MakeMove() =>
      val gameStatus = gameBoard.makeMove()
      if (gameStatus == 0) {
        println("Game over!")
        gameBoard.checkResults()
        endGame()
      }
      else if (gameStatus == 1) {
        println("One more round for player")
        gameBoard.printBoard()
        players(currentPlayer)._2 ! move(gameBoard)
      }
      else if (gameStatus == 2) {
        println("Next player round:")
        gameBoard.printBoard()
        oppositePlayer()
        players(currentPlayer)._2 ! move(gameBoard)
      }

  }
}
