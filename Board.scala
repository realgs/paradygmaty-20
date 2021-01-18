package L7

import scala.util.Random

class Board {

  val NumberOfHouses = 6
  val HousesLenght = 7
  var houses : Array[Array[Int]] = Array.ofDim[Int](2, NumberOfHouses + 1)
  val BaseIndex: Int = 6
  val ExitInput: Int = 2000
  var isGameStarted: Boolean = false
  var isGameEnded: Boolean = false
  var isExtraMove: Boolean = false
  var wasExitInputUsed: Boolean = false
  var r = Random

  def prepareBoard():Unit ={
    for (i <- 0 until BaseIndex){
      houses(0)(i) = 6
      houses(1)(i) = 6
    }
    houses(0)(BaseIndex) = 0
    houses(1)(BaseIndex) = 0
    isGameStarted = true
    isGameEnded = false
    isExtraMove = false
    wasExitInputUsed = false
  }

  def getOppositeToken(playerToken: Int) = (playerToken+1)%2
  def getOppositeIndex(index: Int):Int ={
    NumberOfHouses - 1 - index
  }



  def move_Stones(houseNumber: Int, playerToken: Int): Unit ={
    isExtraMove = false
    if(isGameStarted){
      if(houseNumber != BaseIndex){
        val numberOfSeeds = houses(playerToken)(houseNumber)
        houses(playerToken)(houseNumber) = 0
        for (i <- 1 to numberOfSeeds)
          houses(playerToken)((houseNumber + i) % HousesLenght) = 1 + houses(playerToken)((houseNumber + i) % HousesLenght)

        val lastIndex = (houseNumber + numberOfSeeds) % HousesLenght
        if(lastIndex == BaseIndex){
          isExtraMove = true
        } else if (houses(playerToken)(lastIndex) == 1){
          houses(playerToken)(BaseIndex) = houses(playerToken)(BaseIndex) + 1 + houses(getOppositeToken(playerToken))(getOppositeIndex(lastIndex))
          houses(playerToken)(lastIndex) = 0
          houses(getOppositeToken(playerToken))(getOppositeIndex(lastIndex)) = 0
        }
      }
    }
  }

  def printBoard(): Unit={
    println("Base: " + houses(1).toList.reverse.head +" "+ houses(1).toList.reverse.tail)
    println("        "+houses(0).toList.reverse.tail.reverse+ " Base: " + houses(0).toList.reverse.head+"\n")
  }

  def houseCapture(playerTokenLocal: Int)(index: Int): (Int, Int)= {
    var nextMoveCapture = -1
    var score = 0
    val lastIndex = (index + houses(playerTokenLocal)(index))% HousesLenght
    if(houses(playerTokenLocal)(index) <= 7) {
      if (lastIndex != BaseIndex) {
        if (houses(playerTokenLocal)(lastIndex) == 0 || lastIndex == index) {
          score = houses((playerTokenLocal+1)%2)(getOppositeIndex(lastIndex))
          nextMoveCapture = index
        }
      }
    }
    (nextMoveCapture, score)
  }

  def isGamePossible(playerToken: Int): Boolean ={
    var nonEmptyHouses = 0
    for (i <- 0 until NumberOfHouses){
      if(houses(playerToken)(i) == 0) nonEmptyHouses = nonEmptyHouses+1
    }
    if(nonEmptyHouses == 6) isGameEnded = true
    !isGameEnded
  }

  def printScore(token: Int  = -1): Unit ={
    println("Player0 score: "+houses(0)(BaseIndex))
    println("Player1 score: "+houses(1)(BaseIndex))
    if(wasExitInputUsed) println("Player"+token+" gave up, Player"+ (token+1)%2+" Win")
    else if(houses(0)(BaseIndex) ==houses(1)(BaseIndex)){
      println("Draw")
    }else if(houses(0)(BaseIndex) > houses(1)(BaseIndex)){
      println("Player0 Win")
    }else println("Player1 Win")
  }

  def SeedsToBase(playerToken: Int)= {
    for (i <- 0 until NumberOfHouses) {
      if (houses(playerToken)(i) != 0) {
        houses(playerToken)(BaseIndex) = houses(playerToken)(BaseIndex) + houses(playerToken)(i)
        houses(playerToken)(i) = 0
      }
    }
  }

  def chooseRandomHouse(playerToken: Int) ={
    var randomCorrect: Boolean = false
    var move = 0
    while(!randomCorrect){
      move = r.nextInt(6)
      if(houses(playerToken)(move) != 0) randomCorrect = true
    }
    move
  }

  def EndGame(token: Int = -1)={
    SeedsToBase(0)
    SeedsToBase(1)
    if(wasExitInputUsed) printScore(token)
    else printScore()
  }


}




