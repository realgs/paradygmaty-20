// Konrad Karanowski
package ClientServer

import Engine.Engine.findBestMove
import Game.Board

class AIPlayer(private[this] val isBottom: Boolean, private[this] val maxDepth: Int) extends Player
{

  override def makeMove(board: Board): Int =
  {
    val move = findBestMove(board, isBottom, maxDepth)
    println("Tremble before THIS move!")
    println(s"* computer plays ${move + 1} *")
    move
  }
}
