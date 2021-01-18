package L7

import akka.actor.{Actor, ActorRef, PoisonPill}

import scala.util.Random

class CompPlayer(val server: ActorRef, val playerToken: Int,val hardnessMode: Int) extends Actor{
  var r = Random
  server ! Connect(playerToken)

  override def receive: Receive = {
    case MoveNeeded(board) =>{
      sender ! MakeMove(loadMove(board), playerToken)
    }

    case Disconnect() =>{
      println("Player"+playerToken+ " disconnected")
      self ! PoisonPill
    }

  }

  def loadMove(board: Board): Int ={
    println("Player"+playerToken+ " move")
    if(hardnessMode == 0) findNextMove(board)
    else randomMove(board)
  }

  def randomMove(board: Board): Int={
    val move = board.chooseRandomHouse(playerToken)
    println("random move: "+move)
    move
  }


  def findNextMove(board: Board): Int = {
    var score = 0
    var scoreOp = 0
    var maxSeeds = 0
    var nextMaxSeeds = -1
    var nextMoveCapture = -1
    var nextMoveAntiCapture = -1
    var nextMoveExtra = -1
    Thread.sleep(500)
    for (i <- 0 until board.NumberOfHouses){
      if(board.houses(playerToken)(i) != 0){
        val lastIndex = (i + board.houses(playerToken)(i))% board.HousesLenght
        val (nextMoveCapture_i, score_i) = board.houseCapture(playerToken)(i)
        if (score < score_i) {
          score = score_i
          nextMoveCapture = nextMoveCapture_i
        }
        val (nextMoveCaptureOp_i, scoreOp_i) = board.houseCapture(board.getOppositeToken(playerToken))(i)
        if (scoreOp < scoreOp_i) {
          scoreOp = scoreOp_i
          val index = board.getOppositeIndex((nextMoveCaptureOp_i+board.houses(board.getOppositeToken(playerToken))(nextMoveCaptureOp_i))%board.HousesLenght)
          if(board.houses(playerToken)(index) != 0) nextMoveAntiCapture = index
        }

        if(lastIndex == board.BaseIndex) {
          nextMoveExtra = i
        }
        if (maxSeeds < board.houses(playerToken)(i)) {
          maxSeeds = board.houses(playerToken)(i)
          nextMaxSeeds = i
        }

      }
    }
    if (nextMoveAntiCapture != -1) {
      println("nextMoveAntiCapture " + nextMoveAntiCapture)
      nextMoveAntiCapture
    }
    else if (nextMoveCapture != -1) {
      println("nextMoveCapture " + nextMoveCapture)
      nextMoveCapture
    }
    else if (nextMoveExtra != -1){
      println("nextMoveExtra " + nextMoveExtra)
      nextMoveExtra
    }
    else if (nextMaxSeeds != -1){
      println("nextMaxSeeds " + nextMaxSeeds)
      nextMaxSeeds
    }else nextMaxSeeds
  }
}




