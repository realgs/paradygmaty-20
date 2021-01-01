import akka.actor.{Actor, ActorRef}

class Client(server: ActorRef,playerNumber: Int) extends Actor  {
  override def receive: Receive = {
    case Start => server ! AskForApproval(playerNumber)
    case DemandAccepted => server ! MakeAMove(chooseAHole())
    case DemandRejected => Thread.sleep(1000); server ! AskForApproval(playerNumber)
    case InvalidMove =>  println("Invalid move!"); server ! MakeAMove(chooseAHole())
    case ValidMove => server ! AskForApproval(playerNumber)
  }

  private def chooseAHole():Int = {
    print(s"Player $playerNumber - choose a hole: ")
    scala.io.StdIn.readInt()
  }
}
//Actions handled by Client
case object Start
case object DemandAccepted
case object DemandRejected
case object InvalidMove
case object ValidMove