class Board(private val startingSeedsNumber: Int, val startingPayer: Byte) {
  private val lastHouseIndex = 13
  private val sndPlayerEndZoneIndex = lastHouseIndex
  private val fstPlayerEndZoneIndex = 6

  private var lastDroppedSeedIndex = -1
  private var activePlayerNumber = startingPayer

  val pits : Array[Int] = Array.fill(14)(startingSeedsNumber)
  pits(fstPlayerEndZoneIndex) = 0
  pits(sndPlayerEndZoneIndex) = 0

  def getOppositeHouseIndex(index: Int): Int = {
    lastHouseIndex - 1 - index
  }

  def isHouseEmpty(index: Int): Boolean = {
    pits(index) == 0
  }

  def isChosenPitCorrect(index: Int): Boolean = {
    if(activePlayerNumber == 0){
      if(index > 5 || index < 0) false
      else{
        if(isHouseEmpty(index)) false
        else true
      }
    }
    else{
      if(index > 12 || index < 7) false
      else{
        if(isHouseEmpty(index)) false
        else true
      }
    }
  }

  def moveSeedsFrom(index: Int): Unit ={
    var seeds = pits(index)
    pits(index) = 0
    lastDroppedSeedIndex = index
    while(seeds > 0){
      lastDroppedSeedIndex = (lastDroppedSeedIndex + 1) % 13
      pits(lastDroppedSeedIndex) += 1
      seeds -= 1
    }
  }

  def endGameCollectSeeds(): Unit = {
    if(activePlayerNumber == 0 && pits.slice(0, 6).sum == 0){
      pits(sndPlayerEndZoneIndex) += pits.slice(7, 13).sum
    }
    if(activePlayerNumber == 1 && pits.slice(7, 13).sum == 0){
      pits(fstPlayerEndZoneIndex) += pits.slice(0, 6).sum
    }
  }

  def collectSeedsFromOppositePit(index: Int): Unit = {
    if(activePlayerNumber == 0){
      pits(fstPlayerEndZoneIndex) += (pits(getOppositeHouseIndex(index)) + 1)
    }
    else {
      pits(sndPlayerEndZoneIndex) += (pits(getOppositeHouseIndex(index)) + 1)
    }
    pits(getOppositeHouseIndex(index)) = 0
    pits(index) = 0
  }

  def determineNextPlayerNumber(): Unit = {
    if(activePlayerNumber == 0 && lastDroppedSeedIndex != fstPlayerEndZoneIndex) {
      activePlayerNumber = 1
    }
    if(activePlayerNumber == 1 && lastDroppedSeedIndex != sndPlayerEndZoneIndex) {
      activePlayerNumber = 0
    }
  }

  def isMovePossible: Boolean = {
    if(activePlayerNumber == 0 && pits.slice(0, 6).sum != 0) true
    else if(activePlayerNumber == 1 && pits.slice(7, 13).sum != 0) true
    else false
  }

  def printResult(): Unit = {
    println(s"Player 1 scored: ${pits(fstPlayerEndZoneIndex)}")
    println(s"Player 2 scored: ${pits(sndPlayerEndZoneIndex)}")
    if(pits(sndPlayerEndZoneIndex) > pits(fstPlayerEndZoneIndex)){
      println("Player 2 won!")
    }
    else{
      if(pits(sndPlayerEndZoneIndex) == pits(fstPlayerEndZoneIndex)){
        println("Draw!")
      }
      else {
        println("Player 1 won!")
      }
    }
  }

  def printBoard(): Unit = {
    println("")
    println("Pit nr:\t12\t11\t10\t9\t8\t7\t\tPlayer 2")
    println(s"\t\t[${pits(12)}]\t[${pits(11)}]\t[${pits(10)}]\t[${pits(9)}]\t[${pits(8)}]\t[${pits(7)}]")
    println(s"\t[${pits(13)}]\t\t\t\t\t\t\t\t\t[${pits(6)}]")
    println(s"\t\t[${pits(0)}]\t[${pits(1)}]\t[${pits(2)}]\t[${pits(3)}]\t[${pits(4)}]\t[${pits(5)}]")
    println("Pit nr:\t0\t1\t2\t3\t4\t5\t\tPlayer 1")
  }
}
