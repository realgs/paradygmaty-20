class Board(private val startingSeedsNumber: Int, val startingPayer: Byte) {
  private val lastHouseIndex = 13
  private val sndPlayerEndZoneIndex = lastHouseIndex;
  private val fstPlayerEndZoneIndex = 6

  private var lastDroppedSeedIndex = -1
  private var activePlayerNumber = startingPayer

  val pits : Array[Int] = Array.fill(14)(startingSeedsNumber)
  pits(6) = 0
  pits(13) = 0

  def getOppositeHouseIndex(index: Int): Int = {
    lastHouseIndex - 1 - index
  }

  def isHouseEmpty(index: Int): Boolean = {
    pits(index) == 0
  }

  def isChosenPitCorrect(index: Int): Boolean = {
    if(activePlayerNumber == 0){
      if(index > 5 && index < 0) false
      else true
    }
    else{
      if(index > 12 && index < 7) false
      else true
    }
  }

  def isMovePossible: Boolean = {
    if(activePlayerNumber == 0 && pits.slice(0, 6).sum == 0) true
    else if(activePlayerNumber == 1 && pits.slice(7, 13).sum == 0) true
    else false
  }

  def printBoard(): Unit = {
    println("")
    println(s"\t\t[${pits(12)}]\t[${pits(11)}]\t[${pits(10)}]\t[${pits(9)}]\t[${pits(8)}]\t[${pits(7)}]")
    println(s"\t[${pits(13)}]\t\t\t\t\t\t\t\t\t[${pits(6)}]")
    println(s"\t\t[${pits(0)}]\t[${pits(1)}]\t[${pits(2)}]\t[${pits(3)}]\t[${pits(4)}]\t[${pits(5)}]")
    println("Pit nr:\t0\t1\t2\t3\t4\t5")
  }
}
