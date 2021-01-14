package algorithms

import java.util.concurrent.ForkJoinTask.invokeAll
import java.util.concurrent.{ForkJoinPool, RecursiveTask}

import benchmarks.SumOfFullTreeBenchmark.TREE_DEPTH

object SumOfFullTree {
  def sequential(tree: BT[Int]): Int = {
    def sequentialSum(node: BT[Int]): Int = node match {
      case Empty => 0
      case Node(value, leftBT, rightBT) => {
        if (leftBT == Empty && rightBT == Empty) value
        else value + sequentialSum(leftBT) + sequentialSum(rightBT)
      }
    }

    sequentialSum(tree)
  }

  def parallel(tree: BT[Int]): Int = {
    new ForkJoinPool().invoke(new TreeSumParallel(TREE_DEPTH, tree))
  }

  class TreeSumParallel(currentDepth: Int, tree: BT[Int]) extends RecursiveTask[Int] {
    private val THRESHOLD_TREE_SUM_PARALLEL = 13

    override def compute(): Int = {
      if (currentDepth <= THRESHOLD_TREE_SUM_PARALLEL) {
        sequential(tree)
      }
      else tree match {
        case Empty => 0
        case Node(value, leftBT, rightBT) => {
          if (leftBT == Empty && rightBT == Empty) {
            value
          } else {
            val leftSum = new TreeSumParallel(currentDepth - 1, leftBT)
            val rightSum = new TreeSumParallel(currentDepth - 1, rightBT)

            invokeAll(leftSum, rightSum)

            value + leftSum.get() + rightSum.get()
          }
        }
      }
    }
  }

}

sealed trait BT[+A]

case object Empty extends BT[Nothing]

case class Node[+A](element: A, leftBT: BT[A], rightBT: BT[A]) extends BT[A]