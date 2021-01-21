// Konrad Karanowski
package Game

class Board(boardState: Array[Int])
{
  private[this] val holes: Array[Int] = boardState

  def this(initialNumberOfStones: Int)
  {
    this({
      val board = Array.fill(14)(initialNumberOfStones)
      board(6) = 0
      board(13) = 0
      board
    })
  }

  private def zeroArray(): Unit =
  {
    for (i <- 0 until 14)
      {
        if ((i != 6) && (i != 13))
          {
            holes(i) = 0
          }
      }
  }

  def copy(): Board =
  {
    val newBoard = holes.map(i => i)
    Board(newBoard)
  }

  def getBoardState: Array[Int] = holes

  def bottomPlayerMancala: Int = holes(6)

  def topPlayerMancala: Int = holes(13)

  private def sumRows(idx: Int): Int =
  {
    (idx until 6 + idx).map(i => holes(i)).sum
  }

  def isLegalBottom(idx: Int): Boolean = idx >= 0 && idx < 6 && holes(idx) != 0

  def isLegalTop(idx: Int): Boolean = idx >= 0 && idx < 6 && holes(idx + 7) != 0

  def isFinished: Boolean =
  {
    val bottomScore = sumRows(0)
    val topScore = sumRows(7)
    if (bottomScore == 0)
    {
      holes(13) += topScore
      holes(6) += bottomScore
      zeroArray()
      true
    }
    else if (topScore == 0)
    {
      holes(13) += topScore
      holes(6) += bottomScore
      zeroArray()
      true
    }
    else false
  }

  private def indicatorRange(indicator: Int, shift: Int): Boolean =
  {
    if (shift > 0) indicator > 6 && indicator < 13
    else indicator < 6
  }

  private def playerMove(idx: Int, shift: Int): (Boolean, Int) =
  {
    var numberSteps = holes(idx + shift)
    holes(idx + shift) = 0
    var indicator = idx + shift
    var result = 0
    while (numberSteps > 0)
    {
      indicator = (indicator + 1) % 14
      if(indicator == 6 + shift) result += 1
      // alternative rules
      else if(indicator == 13 - shift) numberSteps += 1
      else
      {
        holes(indicator) += 1
      }
      numberSteps -= 1
    }
    if (indicatorRange(indicator, shift)
      && (holes(indicator) == 1)
      && (holes(12 - indicator) != 0))
    {
      // alternative rules
      result += holes(indicator)
      holes(indicator) = 0
      result += holes(12 - indicator)
      holes(12 - indicator) = 0
    }
    if (indicator == 6 + shift) (true, result)
    else (false, result)
  }

  def bottomPlayerMove(idx: Int): Boolean =
  {

    val (nextMove, score) = playerMove(idx, 0)
    holes(6) += score
    nextMove
  }

  def topPlayerMove(idx: Int): Boolean =
  {
    val (nextMove, score) = playerMove(idx, 7)
    holes(13) += score
    nextMove
  }

  def printBoard(): Unit =
  {
    var string = s"$topPlayerMancala | "
    for(i <- 0 until 6)
    {
      string += s"${holes(12 - i)} "
    }
    string += "|\n  | "
    for(i <- 0 until 6)
    {
      string += s"${holes(i)} "
    }
    string += s"| $bottomPlayerMancala\n"
    print(string)
  }
}

object Board
{
  def apply(initialNumberOfStones: Int): Board = new Board(initialNumberOfStones)

  def apply(boardState: Array[Int]): Board = new Board(boardState)
}
