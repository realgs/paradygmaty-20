package actors

import Board.{Board, ImmutableBoard}
import akka.actor._
import Server._
import engine.KalahaEngine

class Bot(id: Int, private[this] val engine: KalahaEngine) extends Player(id)
{
  override protected def makeMove(board: Board): Int =
    {
      println()
      println(board)
      val move = findBestMove(board)
      println(s"Bot$id choice is: ${move + 1}")
      move
    }

  def findBestMove(board: Board): Int =
    {
      engine.calculateBestMove(new ImmutableBoard(board.copyBoard()))
    }
}

object Bot
{
  def props(id: Int, engine: KalahaEngine): Props = Props(classOf[Bot], id, engine)
}
