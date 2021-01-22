package engine

import Board.ImmutableBoard

trait KalahaEngine
{
  def calculateBestMove(board: ImmutableBoard): Int
}
