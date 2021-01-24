import akka.actor.{Actor, ActorRef, PoisonPill}

case class Move(pit: Int)
case class IsAvailable(pit: Int)
case object ShowBoard
case object Stop
case object Start

class Server(val player1: ActorRef, val player2: ActorRef) extends Actor{

  var board=new Board
  var player=true

  override def receive: Receive = {
    case Start => {
      player=false
      println("Start")
      player2 ! MakeAMove
    }
    case IsAvailable(pit) => {
      if(player) board.isAvailableForPlayer(pit, 1) else board.isAvailableForPlayer(pit, 2)
    }
    case Move(pit) => {
      println("Move "+pit)
      if(pit<0 || pit>5){
        println("Wrong hole!")
        sender ! MakeAMove
      }else{
        board.moveFromHole(pit, player)
        player=(!player)
        if(player){
          player1 ! MakeAMove
        }else{
          player2 ! MakeAMove
        }
      }
    }
    case ShowBoard => {
      println("Showing board")
      board.showBoard()
    }
    case Stop => {
      board.endGame()
      player1 ! PoisonPill
      player2 ! PoisonPill
      board.result()
      self ! PoisonPill
    }
  }
}
