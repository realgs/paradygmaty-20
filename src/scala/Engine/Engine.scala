package Engine

import Game.Board;
import scala.math.{max, min}
import scala.collection.parallel.CollectionConverters._


object Engine
{
  private[this] val maximalValue: Int = 200
  private[this] val minimalValue: Int = -200

  def findBestMove(position: Board, maximizeScore: Boolean, maxDepth: Int): Int =
  {
    val moves = getMoves(position, maximizeScore, maxDepth)
    println(moves.mkString(" | "))
    if (maximizeScore) moves.sortBy(x => x._2)(Ordering[Int].reverse)(0)._1
    else moves.sortBy(x => x._2)(Ordering[Int])(0)._1
  }

  def getMoves(position: Board, maximizeScore: Boolean, maxDepth: Int): Array[(Int, Int)] =
  {
    (0 until 6).par.map(
      move =>
      {
        if(maximizeScore && position.isLegalBottom(move))
          (move, makeMoveMaximize(position, move, minimalValue, maximalValue, maxDepth))
        else if(!maximizeScore && position.isLegalTop(move))
          (move, makeMoveMinimize(position, move, minimalValue, maximalValue, maxDepth))
        else (maximalValue, 0)
      }
    ).filter(x => x._1 != maximalValue).toArray
  }

  private def findBestMoveInner(position: Board, maximizeScore: Boolean, alpha: Int, beta: Int, depth: Int): Int =
  {
    if (depth == 0) evaluatePosition(position)
    else if (position.isFinished)
    {
      val positionScore = evaluatePosition(position)
      if (positionScore > 0) positionScore + maximalValue
      else if(positionScore == 0) 0
      else positionScore + minimalValue
    }
    else if (maximizeScore) maximize(position, alpha, beta, depth)
    else minimize(position, alpha, beta, depth)
  }

  private def minimize(position: Board, alpha: Int, beta: Int, depth: Int): Int =
  {
    @scala.annotation.tailrec
    def minimizeRec(position: Board, bestScore: Int, move: Int, alpha: Int, beta: Int): Int =
    {
      if (move > 5 || beta <= alpha
      ) bestScore
      else
      {
        if (position.isLegalTop(move))
        {
          val score = makeMoveMinimize(position, move, alpha, beta, depth)
          val newBeta = min(beta, score)
          if (score < bestScore) minimizeRec(position, score, move + 1, alpha, newBeta)
          else minimizeRec(position, bestScore, move + 1, alpha, newBeta)
        }
        else minimizeRec(position, bestScore, move + 1, alpha, beta)
      }
    }
    minimizeRec(position, maximalValue, 0, alpha, beta)
  }

  private def maximize(position: Board, alpha: Int, beta: Int, depth: Int): Int =
  {
    @scala.annotation.tailrec
    def maximizeRec(position: Board, bestScore: Int, move: Int, alpha: Int, beta: Int): Int =
    {
      if (move > 5 || beta <= alpha
      ) bestScore
      else
      {
        if (position.isLegalBottom(move))
        {
          val score = makeMoveMaximize(position, move, alpha, beta, depth)
          val newAlpha = max(alpha, score)
          if (score > bestScore) maximizeRec(position, score, move + 1, newAlpha, beta)
          else maximizeRec(position, bestScore, move + 1, newAlpha, beta)
        }
        else maximizeRec(position, bestScore, move + 1, alpha, beta)
      }
    }
    maximizeRec(position, minimalValue, 0, alpha, beta)
  }

  private def makeMoveMaximize(position: Board, move: Int, alpha: Int, beta: Int, depth: Int): Int =
  {
    val board = position.copy()
    val hasNext = board.bottomPlayerMove(move)
    findBestMoveInner(board, hasNext, alpha, beta, depth - 1)
  }

  private def makeMoveMinimize(position: Board, move: Int, alpha: Int, beta: Int, depth: Int): Int =
  {
    val board = position.copy()
    val hasNext = board.topPlayerMove(move)
    findBestMoveInner(board, !hasNext, alpha, beta, depth - 1)
  }

  private def evaluatePosition(position: Board): Int =
  {
    position.bottomPlayerMancala - position.topPlayerMancala
  }
}
