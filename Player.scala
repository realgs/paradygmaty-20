package lab7

import akka.actor.{Actor, ActorRef}
import scala.io.StdIn.readInt

abstract class Player(val side: Int, val server: ActorRef) extends Actor
{
  server ! Connect(side)
  protected val numbOfHouses = 6
  protected val modFactor: Int = numbOfHouses+1
  protected val moveFactor = 1
  protected val baseIdx: Int = 6

  def computeMove(houses: Array[Array[Int]]): Int = 0
  override def receive: Receive =
  {
    case Disconnect() =>context.stop(self)
    case ComputeMove(houses, moveNumb) => server ! MakeMove(computeMove(houses), side, moveNumb)
    case ComputeMoveAgain(houses, moveNumb) =>server ! MakeMove(nonEmptyHouse(houses),side,moveNumb)
  }
  def nonEmptyHouse(houses: Array[Array[Int]]): Int =
  {
      var idx = 0
      for(i<-0 until numbOfHouses){
          if(houses(side)(i) != 0) idx = i
      }
      idx
  }
}
class CompPlayer(side: Int,server: ActorRef) extends Player(side,server)
{
  // Strategy
  override def computeMove(houses: Array[Array[Int]]): Int =
  {
    def lastStoneIdx(idx: Int): Int =
    {
      //return index where last stone from given house will be put
      ((houses(side)(idx) % modFactor) + idx) % modFactor
    }
    def checkEarnStones(): (List[Int], List[Int] )=
    {
      //check if you can take stones from enemy houses
      //return List of indexes with possible earn in stones
      var result = List[Int]()
      var profit = List[Int]()
      for(i<-0 until numbOfHouses)
      {
        if(houses((side+1)%2)(numbOfHouses-1-i) != 0)
        {
          if(houses(side)(i) == 0) {
            //check if start from any index end at this empty house
            for (j <- 0 until numbOfHouses) {
              if (lastStoneIdx(j) == i && j != i && houses(side)(j) <= numbOfHouses+1 && houses(side)(j) != 0){
                result = j :: result
                profit = houses((side+1)%2)(numbOfHouses-1-i)::profit
              }
            }
          }
          else{
            if(lastStoneIdx(i) == i && houses(side)(i) <= numbOfHouses+1){
              result = i::result
              profit = houses((side+1)%2)(numbOfHouses-1-i)::profit
            }
          }
        }
      }
      (result,profit)
    }
    def checkExtraMove(): List[Int] ={
      //checks if you can get extra move
      //return List of indexes with possible extra move
      var result = List[Int]()
      for(i<-0 until numbOfHouses)
      {
        if(lastStoneIdx(i) == baseIdx)result = i::result
      }
      result
    }
    def prepareEmpty(): Int =
    {
      //prepare empty house to take enemy stones in next move
      val canBeEmpty: List[Int] = List[Int]()
      for(i<-1 to numbOfHouses)
      {
        if(houses(side)(numbOfHouses - i) <= numbOfHouses && houses(side)(numbOfHouses - i) != 0) (numbOfHouses - i)::canBeEmpty
      }
      //simple strategy empty house near enemy base
      if(canBeEmpty.nonEmpty) canBeEmpty.head
      else {
        var result = 0
        for(i<-1 to numbOfHouses){
          if(houses(side)(numbOfHouses-i) != 0 && houses((side+1)%2)(i-1) != 0) result = numbOfHouses-i
        }
        result
      }
    }
    def checkBigLost(): (List[Int],List[Int]) =
    {
      //check if enemy can take your big amount of stones from one house
      var dangered: List[Int] = List[Int]()
      var loss: List[Int] = List[Int]()
      for(i<-0 until numbOfHouses)
      {
        if(houses(side)(i) > numbOfHouses && houses((side+1)%2)(numbOfHouses-1-i) == 0){
          dangered = i::dangered
          loss = houses(side)(i)::loss
        }
      }
      (dangered, loss)
    }
    def getTheBestOne(indexes: List[Int],value: List[Int]): Int =
    {
      var most = 0
      var index = 0
      for(i<-value.indices)
      {
        if(value.apply(i)>= most) {
          most = value.apply(i)
          index = indexes.apply(i)
        }
      }
      index
    }
    val (earn,profit) = checkEarnStones()
    val (dangered, loss) = checkBigLost()
    val extraMove = checkExtraMove()
    val empty = prepareEmpty()
    if(earn.nonEmpty) getTheBestOne(earn,profit)
    else{
      if(extraMove.nonEmpty) extraMove.head
      else{
        if(dangered.nonEmpty) getTheBestOne(dangered,loss)
        else empty
      }
    }
  }
}
class HumanPlayer(side: Int,server: ActorRef) extends Player(side, server)
{
  private var houseIdx = 0
  override def computeMove(houses: Array[Array[Int]]): Int =
  {
    println("Player "+side+" please enter house number 0-5 , 7 for auto move, 8 to end the game")
    print("Number: ")
    houseIdx = readInt()
    def checkValidIndex(): Unit =
    {
      while(houseIdx > 5 || houseIdx < 0)
      {
        println("Not valid house index, enter number between 0 and 5")
        print("Number: ")
        houseIdx = readInt()
      }
    }
    def checkEmptyHouse(): Unit =
    {
      while(houses(side)(houseIdx) == 0)
      {
        println("There are no stones in this house, choose another")
        print("Number: ")
        houseIdx = readInt()
        checkValidIndex()
      }
    }
    if(houseIdx == 7 || houseIdx == 8) houseIdx
    else {
      checkValidIndex()
      checkEmptyHouse()
      houseIdx
    }
  }
}
