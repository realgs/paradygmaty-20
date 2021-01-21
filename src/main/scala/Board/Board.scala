package Board

class Board
{
  val board: Array[Array[Int]] = Array(Array(6, 6, 6, 6, 6, 6, 0), Array(6, 6, 6, 6, 6, 6, 0))

  override def toString: String =
    {
      val builder: StringBuilder = new StringBuilder
      builder.addOne(' ')
      for (_ <- 0 to 5)
        builder.addAll(" " + board(0).toString)
      builder.addOne('\n')
      builder.addAll(board(0)(5).toString + "            " + board(1)(5))
      builder.addOne('\n')
      builder.addOne(' ')
      for (_ <- 0 to 5)
        builder.addAll(" " + board(1).toString)

      builder.result()
    }

}
