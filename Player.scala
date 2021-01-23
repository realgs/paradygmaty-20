import akka.actor.{Actor, ActorRef, PoisonPill}

class Player(server: ActorRef, playerNumber: Int) extends Actor {
  private var playerID: Int = playerNumber
  server ! Connect(playerID)

  override def receive: Receive = onMessage(playerID)

  def getMove(gameBoard: GameBoard): Int = {
    print("Type number of hole: ")
    val move = scala.io.StdIn.readInt()
    if(move >= 0 && move <=6 && gameBoard.checkMove(move, playerID)) move
    else {
      println("Invalid move, try again")
      getMove(gameBoard)
    }
  }

  private def onMessage(playerID: Int): Receive = {
    case move(gameBoard: GameBoard) =>
      println(s"Your turn Player $playerID: ")
      getMove(gameBoard)
      server ! MakeMove()

    case Disconnect() =>
      self ! PoisonPill
  }
}
