package L7

import akka.actor.{Actor, ActorRef}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.util.Random

class Server extends Actor {
  var r = Random
  var board: Board = new Board
  var players: Array[ActorRef] = new Array[ActorRef](2)
  var numberOfPlayers = 0
  var moveDone :Boolean = true
  var token = 0
  var timeStart: Long = 0
  var timeEnd: Long = 0
  var isTimeUp: Boolean = false
  val EXIT_INPUT = 2000


  override def receive: Receive = {

    case Connect(playerToken) => {
      players(playerToken) = sender
      context.watch(sender)
      println("Player" + playerToken + " connected")
      numberOfPlayers += 1
      if (numberOfPlayers == 2) startGame()
    }

    case MakeMove(houseNumber: Int, playerToken: Int) => {
      if(houseNumber == EXIT_INPUT){
        board.wasExitInputUsed = true
        endGame
      }
      else if(isTimeUp){
        isTimeUp = false
        makeRandomMove
        nextMove
      }
      else if (checkMove(houseNumber, playerToken)) {
            board.move_Stones(houseNumber, playerToken)
            board.printBoard()
            nextMove
        } else moveControl(true)
      }

    case timeIsOver() =>{
      if(!checkTime) {
        isTimeUp = true
        println("Enter any numeric value to continue, instant exit code 2000")
        print("Number: ")
      }
    }
  }

  def checkMove(houseNumber: Int, playerToken : Int): Boolean ={
    if(houseNumber>5 || houseNumber<0){
      println("Bad value of houseNumber")
      false
    }else if(board.houses(playerToken)(houseNumber) == 0){
      println("Choosen house is empty, choose other")
      false
    }else true
  }

  def checkTime: Boolean ={
    timeEnd = System.nanoTime()
    if((timeEnd-timeStart) >30000000000L){
      println("Time to make move is over")
      false
    }else true
  }

  def makeRandomMove: Unit={
    val move = board.chooseRandomHouse(token)
    println("Random move for Player"+token+" move: "+move)
    board.move_Stones(move, token)
    board.printBoard()
  }

  def drawFirst: Int ={
    r.nextInt(30)%2
  }


  def startGame():Unit =
    if (numberOfPlayers == 2) {
      board.prepareBoard()
      board.printBoard()
      token = drawFirst
      println("Player"+token+" starts the game!")
      moveControl(false)
    }

  def endGame :Unit =
    if(numberOfPlayers == 2) {
      if(board.wasExitInputUsed) board.EndGame(token)
      else board.EndGame()
      players(0) ! Disconnect
      players(1) ! Disconnect
      context.system.terminate()
    }

  def moveControl(incorrectMove: Boolean): Unit={
    if(board.isGamePossible(token)){
      if(!incorrectMove){
        timeStart = System.nanoTime()
        context.system.scheduler.scheduleOnce(30.seconds, self, timeIsOver())
      }
      players(token) ! MoveNeeded(board)
    }else endGame
  }

  def nextMove: Unit ={
    if(!board.isExtraMove) {
      println("change of player")
      token = board.getOppositeToken(token)
    }else println("player"+token+" extra move")
    moveControl(false)
  }
}




