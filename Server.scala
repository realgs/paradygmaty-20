import akka.actor.{Actor, ActorRef}

import scala.util.Random

class Server extends Actor {
  private val players: Array[ActorRef] = new Array[ActorRef](2)
  private var currentPlayer: Int = drawRound()
  private val gameBoard: GameBoard = new GameBoard(currentPlayer)

  override def receive: Receive = onMessage(players)

  private def drawRound(): Int = Random.between(0, 2)

  private def nextPlayer(): Unit =
    currentPlayer match {
      case 1 => currentPlayer = 0
      case 0 => currentPlayer = 1
    }

  private def endGame() = context.system.terminate()

  private def onMessage(players: Array[ActorRef]): Receive = {
    case Connect(playerID: Int) =>
      players(playerID) = sender()
      context.watch(sender)
      if (!players.contains(null))
        startGame()

    case MakeMove(move: Int) =>
      val gameStatus = gameBoard.makeMove(move)
      gameStatus match {
        case 0 =>
          gameBoard.printBoard()
          println(Console.RED + "Game over!")
          gameBoard.finishGame()
          endGame()
        case 1 =>
          Thread.sleep(1000)
          gameBoard.printBoard()
          Thread.sleep(1000)
          println(Console.BLUE + s"One more round for Player $currentPlayer" + Console.RESET)
          players(currentPlayer) ! Move(gameBoard)
        case 2 =>
          Thread.sleep(1000)
          gameBoard.printBoard()
          nextPlayer()
          Thread.sleep(1000)
          println(Console.BLUE + s"Next round for Player $currentPlayer" + Console.RESET)
          players(currentPlayer) ! Move(gameBoard)
      }
  }

  private def startGame(): Unit = {
    println("Starting game!")
    Thread.sleep(1000)
    gameBoard.printBoard()
    Thread.sleep(1000)
    players(currentPlayer) ! Move(gameBoard)
  }
}
