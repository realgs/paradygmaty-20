import akka.actor.{Actor, ActorRef, PoisonPill}

class Player(server: ActorRef, playerNumber: Int) extends Actor {
  private val playerID: Int = playerNumber
  server ! Connect(playerID)

  override def receive: Receive = onMessage(playerID)

  def getMove(gameBoard: GameBoard): Int = {
    print("Type number of hole: ")
    val move = scala.io.StdIn.readInt()
    if(move >= 0 && move <6 && gameBoard.checkMove(move)) move
    else {
      println("Invalid move, try again")
      getMove(gameBoard)
    }
  }

  private def onMessage(playerID: Int): Receive = {
    case Move(gameBoard: GameBoard) =>
      println(s"Your turn Player $playerID: ")
      sender() ! getMove(gameBoard)

    case Disconnect() =>
      self ! PoisonPill
  }
}
