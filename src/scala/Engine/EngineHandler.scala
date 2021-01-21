// Konrad Karanowski
package Engine

import Engine.getMoves
import Game.Board

class EngineHandler(depth: Int)
{
  private[this] val maxDepth: Int = depth

  def analyzeBoardBottom(position: Board): Unit =
  {
    val lines = getMoves(position, maximizeScore = true, maxDepth)
    printBestLines(lines)
  }

  def analyzeBoardTop(position: Board): Unit =
  {
    val lines = getMoves(position, maximizeScore = true, maxDepth)
    printBestLines(lines)
  }

  private def printBestLines(lines: Array[(Int, Int)]): Unit =
  {
    println("Best lines for you: ")
    println("============================")
    lines.foreach(line => println(f"Move ${line._1 + 1} => ${line._2}"))
    println("============================")
  }
}

object EngineHandler
{
  def apply(depth: Int): EngineHandler = new EngineHandler(depth)
}
