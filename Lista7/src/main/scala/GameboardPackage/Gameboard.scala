package GameboardPackage

import scala.util.Random

class Gameboard() {
  private val board = createBoard(6)
  private val whoseRound = drawWhoseRound

  def createBoard(numberOfStones: Int): Array[Int] = {
    var i = 0
    while(i < 13) if (i+1 % 7 == 0) i ::
  }

  def drawWhoseRound(): Int = {
    val random = new Random()
    random.nextInt(2)
  }
  def endGameCheck(): Boolean = {
    if (board.slice(0,6).sum == 0 && whoseRound == 1) true
    else if (board.slice(7,13).sum == 0 && whoseRound == 2) true
    else false
  }
}
