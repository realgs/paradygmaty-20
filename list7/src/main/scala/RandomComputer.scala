import scala.util.Random


class RandomComputer(num: Int) extends Player(num) {
  var random = new Random()

  override def makeMove(board: Board): Int = {
    var choice = -1
    if (number == 1) {
      choice = getRandomMove(board, 0, 5)
    }
    else choice = getRandomMove(board, 7, 12)
    println(s"\nComputer's choice: $choice")
    return choice
  }

  def getRandomMove(board: Board, start: Int, end: Int): Int =
  {
    var availableMoves: List[Int] = List()
    for (i <- start to end) {
      if (!board.get(i).isEmpty) {
        availableMoves = i :: availableMoves
      }
    }
    if(availableMoves.nonEmpty) availableMoves(Random.nextInt(availableMoves.length))
    else return -1
  }

}
