import akka.actor.Actor

import scala.util.Random
class Server private(game: Game) extends Actor {
  override def receive: Receive = {
    case AskForApproval(player) => if(game.isGameOver) {game.declareWinner();context.system.terminate()} else if(game.turnOfPlayer(player)) {game.displayBoard();sender() ! DemandAccepted} else sender() ! DemandRejected
    case MakeAMove(hole) => if(game.move(hole)) sender() ! ValidMove else sender() ! InvalidMove
    case MakeRandomMove => makeRandomMove(); sender() ! ValidMove
    case GetGameStatus => sender() ! ObtainGameStatus(game.clone())
  }


  private def makeRandomMove():Unit = {
  {
    var randomMove = Random.nextInt(14)
    while(!game.move(randomMove)) randomMove = Random.nextInt(14)
    println(s"Server made a random move: $randomMove")
  }
  }
}

object Server {
  def apply(game: Game):Server = new Server(game)
}

//Actions handled by Server
case class MakeAMove(hole: Int)
case class AskForApproval(player: Int)
case object MakeRandomMove
case object GetGameStatus