import akka.actor.{Actor, ActorRef, PoisonPill}

import scala.util.Random

class Computer(server: ActorRef, playerNumber: Int) extends Actor {
  private var playerID: Int = playerNumber
  private val random: Random = new Random()

  server ! Connect(playerID)

  override def receive: Receive = onMessage(playerID)

  private def onMessage(playerID: Int): Receive = {
    case move(gameBoard: GameBoard) =>
      println(s"Your turn Player $playerID: ")
      getMove(gameBoard)
      server ! MakeMove()

    case Disconnect() =>
      self ! PoisonPill
  }

  private def getMove(gameBoard: GameBoard): Int = {
    val move: Int = random.between(0, 6)
    if(gameBoard.checkMove(move, playerID)) {
      println(s"Computer move: $move")
      return move
    }
    getMove(gameBoard)
  }

}
