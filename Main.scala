package L7

import akka.actor._

object Main extends App {

  //Prezentowana gra to Kalaha (6, 6), 6 dołków z początkową zawartością 6 nasion/kamieni.
  //gra jest w wariancie "Empty Capture"

  //HardnessMode 0-> Greedy Computer Strategy
  //            other -> Random Move

  def playWithComputer(HardnessMode: Int):Unit ={
    val system = ActorSystem("Kalaha")
    val server: ActorRef = system.actorOf(Props[Server],"Server")
    val Player0: ActorRef  = system.actorOf(Props(classOf[HumanPlayer],server, 0))
    val Player1: ActorRef  = system.actorOf(Props(classOf[CompPlayer],server, 1, HardnessMode))
  }
  def ComputersPlaySimulation(Comp0Mode: Int, Comp1Mode : Int): Unit = {
    val system = ActorSystem("Kalaha")
    val server: ActorRef = system.actorOf(Props[Server],"Server")
    val Player0: ActorRef  = system.actorOf(Props(classOf[CompPlayer],server, 0, Comp0Mode))
    val Player1: ActorRef  = system.actorOf(Props(classOf[CompPlayer],server, 1, Comp1Mode))
  }

  ComputersPlaySimulation(0, 1)
  //playWithComputer(0)
}



