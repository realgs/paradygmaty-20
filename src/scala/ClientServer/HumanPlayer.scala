// Konrad Karanowski
package ClientServer

import Game.Board

class HumanPlayer(private[this] val playerName: String) extends Player
{

  override protected def makeMove(board: Board): Int =
  {
    println(s"$playerName make a move (1-6): ")
    try scala.io.StdIn.readInt() - 1
    catch
    {
      case e : Exception => 10
    }
  }
}
