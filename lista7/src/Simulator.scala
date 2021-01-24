import scala.util.control.Breaks.{break, breakable}

class Simulator(private val board: Board = new Board()) {
  def simulate(position: Int, player: Int): Int = {
    if (board.getPits(player)(position).get() == 0) Int.MinValue
    else simulatorHelper(board.copy(), position, player)
  }

  private def simulatorHelper(localBoard: Board, position: Int, player: Int): Int = {
    val success = move(localBoard, position, player)._1
    val player2 = {
      player match {
        case 0 => 1
        case 1 => 0
        case _ => throw new IllegalArgumentException("Player can only have value 0 or 1")
      }
    }
    if (success) localBoard.getMancala(player).get() - localBoard.getMancala(player2).get()
    else Int.MinValue
  }

  protected def move(localBoard: Board, position: Int, player: Int): (Boolean, Boolean) = {
    if (localBoard.getPits(player)(position).get() == 0) {
      return (false, false)
    }
    val player2 = {
      player match {
        case 0 => 1
        case 1 => 0
        case _ => throw new IllegalArgumentException("Player can only have value 0 or 1")
      }
    }

    var (seeds, lastPitIndex, anotherMove) = moveHelper(localBoard, position, player)
    breakable {
      while (seeds > 0) {
        seeds = updatePitsWithoutMancala(localBoard, seeds, 0, player2)._1
        lastPitIndex = -1
        if (seeds == 0) break
        else {
          val x = updatePitsWithMancala(localBoard, seeds, 0, player)
          seeds = x._1
          lastPitIndex = x._2
        }
      }
    }
    steal(localBoard, lastPitIndex, player, player2)
    (true, anotherMove)
  }

  def moveHelper(localBoard: Board, position: Int, player: Int): (Int, Int, Boolean) =
    updatePitsWithMancala(localBoard, localBoard.getPits(player)(position).take(), position + 1, player)

  def steal(localBoard: Board, lastPitIndex: Int, who: Int, from: Int): Unit = {
    if (lastPitIndex != -1 && getPit(localBoard, who, lastPitIndex) == 1 && getPit(localBoard, from, 5 - lastPitIndex) != 0)
      localBoard.getMancalas(who).addAll(takePit(localBoard, from, 5 - lastPitIndex)
        + takePit(localBoard, who, lastPitIndex))
  }

  private def getPit(localBoard: Board, player: Int, index: Int): Int = localBoard.getPits(player)(index).get()

  private def takePit(localBoard: Board, player: Int, index: Int): Int = localBoard.getPits(player)(index).take()

  private def updatePitsWithoutMancala(localBoard: Board, amount: Int, startHoleIndex: Int, player: Int): (Int, Int) = {
    var seeds = amount
    var pitIndex = startHoleIndex
    while (seeds > 0 && pitIndex < 6) {
      localBoard.getPits(player)(pitIndex).add()
      seeds -= 1
      pitIndex += 1
    }
    //-1 because I return the index which was the last updated one
    (seeds, pitIndex - 1)
  }

  private def updatePitsWithMancala(localBoard: Board, amount: Int, startPitIndex: Int, player: Int): (Int, Int, Boolean) = {
    var anotherMove = false
    var (seeds, pitIndex) = updatePitsWithoutMancala(localBoard, amount, startPitIndex, player)
    if (seeds > 0) {
      localBoard.getMancalas(player).add()
      seeds -= 1
      if (seeds == 0) {
        anotherMove = true
        pitIndex = -1
      }
    }
    (seeds, pitIndex, anotherMove)
  }
}
