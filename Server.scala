package Kalaha
import akka.actor.{Actor, ActorRef, Terminated}
import scala.util.Random

class Server extends Actor {

  private var players = List[(Int, ActorRef)]()
  private var board: Board = _

  //Drawing who starts the game
  private var turn: Int = drawTurn()

  def resetGame(): Board = {
    board = new Board(4)
    board
  }

  def startGame(): Unit = {
    resetGame()
  }

  def receive: Receive = {
    case Connect() =>
      players = (players.size+1, sender) :: players
      context.watch(sender)
      Thread.sleep(1000)
      println("Player: " + players.head._1 + " are ready to game.")
      sender ! ReturnPlayerNumber(players.head._1)

    case Start(player) =>
      println("The game starts by player number " + turn)
      if(turn == getPlayerNumber(player)) {
        startGame()
        board.printBoard
        if(turn == 1) players.tail.head._2 ! RequireMove(turn, board)
        else players.head._2 ! RequireMove(turn, board)
      }

    case MakeMove(holeNumber, playerNumber) =>
      println("Server receive the move from player number: " + playerNumber)
      val ifNextMove = board.makeMove(holeNumber, playerNumber)
      board.printBoard
      val ifEnd = checkIfGameEnded()
      if(ifEnd) {
        println("Game over.")
        endGame()
        players.head._2 ! Disconnect
        players = players.tail
        players.head._2 ! Disconnect
        players = players.tail
      }
      if(ifNextMove == 1) sender ! TurnAgainPlayer(board)
      else if(ifNextMove == 0) {
        println("Invalid move: try again!")
        sender ! InformInvalidMove(board, playerNumber)
      }
      else {
        var opponent: ActorRef = sender
        var opponentNumber = 0
        if(players.head._1 == playerNumber) {
          opponent = players.tail.head._2
          opponentNumber = players.tail.head._1
        }
        else {
          opponent = players.head._2
          opponentNumber = players.head._1
        }
        opponent ! RequireMove(opponentNumber, board)
      }

    case EndGame() =>
      board.printResults
      players.head._2 ! Disconnect
      players.head._2 ! Disconnect

    case TimesUp(playerNumber) =>
      println("Times up, next opponent.")
      var opponent: ActorRef = sender
      var opponentNumber = 0
      if(players.head._1 == playerNumber) {
        opponent = players.tail.head._2
        opponentNumber = players.tail.head._1
      }
      else {
        opponent = players.head._2
        opponentNumber = players.head._1
      }
      opponent ! RequireMove(opponentNumber, board)

    case Terminated(player) =>
      context.stop(self)
      resetGame()
      players = players.filter(sender != _._2)
      Thread.sleep(1000)
      Main.printMenu()
      val number = Main.chooseOption()
      if(number == 1) Main.choice1()
      else if(number == 2) Main.choice2()
      context.system.terminate()
  }

  def getPlayerNumber(actor: ActorRef): Int = {
    players.filter(actor == _._2).head._1
  }

  def drawTurn(): Int = {
    val r: Random = new Random()
    val number = r.between(1, 3)
    number
  }

  def checkIfGameEnded(): Boolean = {
    board.ifEnd(1) || board.ifEnd(2)
  }

  def endGame(): Unit = {
    board.printResults
  }

}
