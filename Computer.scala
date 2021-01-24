import akka.actor.{Actor, ActorRef, PoisonPill}

import scala.util.Random

class Computer(server: ActorRef, playerNumber: Int) extends Actor {
  private val playerID: Int = playerNumber
  private val random: Random = new Random()

  server ! Connect(playerID)

  override def receive: Receive = onMessage(playerID)

  private def onMessage(playerID: Int): Receive = {
    case Move(gameBoard: GameBoard) =>
      println(s"Your turn Player $playerID: ")
      sender() ! getMove(gameBoard)

    case Disconnect() =>
      self ! PoisonPill
  }

  @scala.annotation.tailrec
  private def getMove(gameBoard: GameBoard): Int = {
    val move: Int = random.between(0, 6)
    if(gameBoard.checkMove(move)) {
      println("Computer move:" + Console.RED+ s"$move")
      return move
    }
    getMove(gameBoard)
  }

}
