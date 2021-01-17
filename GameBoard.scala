package lab7

class GameBoard(val numberOfStones: Int)
{
  val numbOfHouses = 6
  val houses: Array[Array[Int]] = Array.ofDim[Int](2,numbOfHouses+1) // Base at index 6
  val baseIdx: Int = 6
  var extraMove = false // true if current player has extra move
  protected val modFactor = numbOfHouses+1
  protected val moveFactor = 1
  var gameON = true

  def fillBoard(): Unit ={
    for(j<-0 until 2){
      for(i<-0 until numbOfHouses)
      {
        houses(j)(i) = numberOfStones
      }
      houses(j)(baseIdx) = 0
    }
  }
  def getResults(): Unit={
    println("Game Over !!! ")
    var result = "Draw"
    if(houses(0)(baseIdx) > houses(1)(baseIdx)) result = "Player 0 won"
    if(houses(0)(baseIdx) < houses(1)(baseIdx)) result = "Player 1 won"
    println("Result "+result+", P0 "+ houses(0)(baseIdx)+", P1 "+ houses(1)(baseIdx))
  }
  def takeAllStones(side : Int): Unit =
  {
    for(i<-0 until numbOfHouses){
      houses(side)(baseIdx) += houses(side)(i)
    }
  }
  def isMovePossible(side: Int): Unit =
  {
    var emptyHouses = 0
    for(i<-0 until numbOfHouses)
    {
      if(houses(side)(i) == 0) emptyHouses += 1
    }
    gameON = emptyHouses != numbOfHouses
  }
  def moveStones(houseNumb: Int, side: Int): Unit =
  {
    var index = houseNumb
    println("Player "+ side+" moved stones from house "+ index)
    val numbOfStones = houses(side)(index)
    houses(side)(index) = 0
    for(i<-1 to numbOfStones)
    {
      index = (index+moveFactor)%modFactor
      houses(side)(index) += 1
      if(i == numbOfStones)
      {
        if(index == baseIdx){
          //extra move
          extraMove = true
        }
        else if(houses(side)(index) == 1)
        {
          // take enemy stones
          println(houses((side+1)%2)(numbOfHouses-1-index)+" stones taken from P"+(side+1)%2+" to P"+side)
          houses(side)(baseIdx) += houses((side+1)%2)(numbOfHouses-1-index) + houses(side)(index)
          houses((side+1)%2)(numbOfHouses-1-index) = 0
          houses(side)(index) = 0
        }
      }
    }
  }
  def printBoard(): Unit=
  {
    println(" Player "+ 0)
    printArray(0)
    println(" Player "+ 1)
    printArray(1)
  }

  def printArray(side: Int): Unit =
  {
    for(i<-0 until numbOfHouses){
      print(houses(side)(i)+", ")
    }
    print(" Base "+houses(side)(baseIdx)+"\n")
  }
  def move(houseNumb: Int, side: Int): Unit =
  {
    println("Player " + side)
    moveStones(houseNumb, side)
    printBoard()
    println()
  }
}
