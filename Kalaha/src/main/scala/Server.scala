import akka.actor.Actor
//TODO handle timeouts
class Server private(board: Game) extends Actor {
  override def receive: Receive = {
    case AskForApproval(player) => if(board.isGameOver) {board.declareWinner();context.system.terminate()} else if(board.turnOfPlayer(player)) {board.displayBoard();sender() ! DemandAccepted} else sender() ! DemandRejected
    case MakeAMove(hole) => if(board.move(hole)) sender() ! ValidMove else sender() ! InvalidMove
  }
}

object Server {
  def apply(game: Game):Server = new Server(game)
}

//Actions handled by Server
case class MakeAMove(hole: Int)
case class AskForApproval(player: Int)