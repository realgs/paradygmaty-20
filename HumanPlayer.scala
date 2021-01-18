package L7

import akka.actor.{Actor, ActorRef, PoisonPill}

import scala.io.StdIn.readInt


class HumanPlayer(server: ActorRef, playerToken: Int) extends Actor {

  server ! Connect(playerToken)
  override def receive: Receive = {

    case MoveNeeded(board) =>{
      println("Player"+playerToken+ " move")
      sender ! MakeMove(loadMove(board), playerToken)
    }

    case Disconnect() =>{
      self ! PoisonPill
    }
  }

  def loadMove(board: Board): Int ={
    var houseNumber = -1
    println("Enter house number, instant exit code: 2000")
    print("Number: ")
    houseNumber = readInt()
    houseNumber
  }
}





