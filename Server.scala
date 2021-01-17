package lab7

import akka.actor.{Actor, ActorRef, Props}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{DurationInt, FiniteDuration}

class Server extends Actor{
  private val gameBoard = new GameBoard(6)
  private val players : Array[ActorRef] = new Array[ActorRef](2)
  private val maxMoveTime: FiniteDuration = 10.seconds
  private var Idx = 0
  private var tokenMove  = 0  // represent who is moving
  private var currentMoveNumb = 0
  private val AutoMoveCode: Int = 7
  private val EndGameCode: Int = 8

  override def receive: Receive = {
    case Connect(side)=>
        if(Idx < 2) {
          players(side) = sender()
          Idx += 1
        }
    case StartGame()=>
        if(Idx == 2) {
          gameBoard.fillBoard()
          players(tokenMove) ! ComputeMove(gameBoard.houses, currentMoveNumb)
          context.system.getScheduler.scheduleOnce(maxMoveTime,self,TimesUp(tokenMove, currentMoveNumb))
        }
        else context.system.terminate()
    case EndGame()=>
    {
        gameBoard.gameON = false
        gameOver()
    }
    case MakeMove(houseNumb, side,moveNumb)=> {
      if (side == tokenMove && currentMoveNumb == moveNumb) {
        if(houseNumb == AutoMoveCode || houseNumb == EndGameCode) {
          if(houseNumb == AutoMoveCode) {
            gameBoard.move(getFirstNotEmptyIdx(side), side)
            continueGame()
          }
          else{
            self ! EndGame()
          }
        }
        else{
          if (isCorrectHouseNumber(houseNumb, side)) {
            gameBoard.move(houseNumb, side)
            continueGame()
          }
          else {
          println("Player " + side + " you selected incorrect houseNumb " + houseNumb + ", chose again")
            sender() ! ComputeMoveAgain(gameBoard.houses,currentMoveNumb)
          }
        }
      }
    }
    case TimesUp(side, moveNumb) =>
    {
      //println("Player "+side +" moveNumb "+moveNumb+" cm "+currentMoveNumb)
      if(currentMoveNumb == moveNumb){
        println("Player "+side +" your move: "+moveNumb+" time end, auto house chosen")
        gameBoard.move(getFirstNotEmptyIdx(side), side)
        println("If Human Player, please enter 7")
        Thread.sleep(5000)
        continueGame()
      }
    }
  }
  def isCorrectHouseNumber(houseNumb: Int, side: Int): Boolean =
  {
      if(houseNumb == gameBoard.baseIdx || houseNumb >= gameBoard.numbOfHouses || houseNumb < 0) false
      else if(gameBoard.houses(side)(houseNumb) <= 0) false
      else true
  }
  def gameOver(): Unit ={
      gameBoard.takeAllStones(0)
      gameBoard.takeAllStones(1)
      gameBoard.getResults()
      context.system.terminate()
  }
  def continueGame(): Unit ={
    currentMoveNumb+=1
    if (!gameBoard.extraMove) tokenMove = (tokenMove + 1) % 2
    else {
      gameBoard.extraMove = false
      println("Extra Move!!!")
    }
    gameBoard.isMovePossible(tokenMove)
    if (gameBoard.gameON) {
      players(tokenMove) ! ComputeMove(gameBoard.houses,currentMoveNumb)
      context.system.getScheduler.scheduleOnce(maxMoveTime,self,TimesUp(tokenMove,currentMoveNumb))
    }
    else gameOver()
  }
  def getFirstNotEmptyIdx(side: Int): Int ={
    var idx = 0
    for(i<-0 until gameBoard.numbOfHouses){
      if(gameBoard.houses(side)(i) != 0) idx = i
    }
    idx
  }
}
object Server
{
    def props(): Props = Props(classOf[Server])
}
