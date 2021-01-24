import akka.actor.{Actor, ActorRef, PoisonPill}

import scala.util.Random

case class Move(pit: Int)
case class IsAvailable(pit: Int)
case class GetMyHoles()
case object ShowBoard
case object Stop
case object Start

class Server(val player1: ActorRef, val player2: ActorRef) extends Actor{

  var board=new Board
  var player=true
  var time1=System.currentTimeMillis()

  override def receive: Receive = {
    case Start => {
      player=false
      time1=System.currentTimeMillis()
      player2 ! MakeAMove
    }
    case IsAvailable(pit) => {
      if(player) board.isAvailableForPlayer(pit, 1) else board.isAvailableForPlayer(pit, 2)
    }
    case Move(pit) => {
      if((System.currentTimeMillis()-time1)>30000){
        println("Game aborted because of time!")
        self ! Stop
      }
      println("Move "+pit)
      if(pit<0 || pit>5){
        board.moveFromHole((new Random).nextInt(6), player)
      }else{
        board.moveFromHole(pit, player)
        player=(!player)
        if(player){
          time1=System.currentTimeMillis()
          player1 ! MakeAMove
        }else{
          time1=System.currentTimeMillis()
          player2 ! MakeAMove
        }
      }
    }
    case ShowBoard => {
      board.showBoard()
      if(player) sender ! UpdateHoles(board.playerOneHoles) else sender ! UpdateHoles(board.playerTwoHoles)
    }
    case Stop => {
      board.endGame()
      player1 ! PoisonPill
      player2 ! PoisonPill
      board.result()
      self ! PoisonPill
      context.system.terminate()
    }
  }

}
