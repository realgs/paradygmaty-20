

class Player(val number: Int){

  def makeMove(board: Board): Int = {
    if (number == 1) print("\nChoose a pit from 0 to 5: ")
    else print("\nChoose a pit from 7 to 12: ")
    val choice = scala.io.StdIn.readInt()
    println(s"\nPlayer $number's choice: $choice")
    return choice
  }

}
