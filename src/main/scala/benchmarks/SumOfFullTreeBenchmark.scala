package benchmarks

import algorithms.{BT, Empty, Node, SumOfFullTree}
import benchmarks.Benchmark.calculateExecutionTime
import scala.util.Random

/*
  Ratio = Sequential time / Parallel time

  Example results:

  WITHOUT THRESHOLD

  Depth: 5
  Sequential time: 1,34 ms
  Parallel time: 3,17 ms
  Ratio: 0,42

  Depth: 10
  Sequential time: 1,41 ms
  Parallel time: 4,31 ms
  Ratio: 0,32

  WITH THRESHOLD

  Depth: 15
  Sequential time: 4,37 ms
  Parallel time: 3,77 ms
  Ratio: 1,16

  Depth: 20
  Sequential time: 13,10 ms
  Parallel time: 5,94 ms
  Ratio: 2,20

  Depth: 22
  Sequential time: 23,42 ms
  Parallel time: 10,36 ms
  Ratio: 2,26

  Depth: 24
  Sequential time: 104,18 ms
  Parallel time: 37,97 ms
  Ratio: 2,74

  Depth: 26
  Sequential time: 300,48 ms
  Parallel time: 103,77 ms
  Ratio: 2,89

  As well as in other algorithms, the number of elements in the tree plays the main role in the
  efficiency of the parallel algorithm.
*/

object SumOfFullTreeBenchmark {

  val TREE_DEPTH = 25
  private val LOWER_RANDOM_NUMBER_BOUND = 10
  private val UPPER_RANDOM_NUMBER_BOUND = 1000

  def run(): Unit = {
    val testTree = generateRandomTree(TREE_DEPTH, LOWER_RANDOM_NUMBER_BOUND, UPPER_RANDOM_NUMBER_BOUND)

    calculateExecutionTime("SumOfFullTreeSequential") {
      SumOfFullTree.sequential(testTree)
    }

    calculateExecutionTime("SumOfFullTreeParallel") {
      SumOfFullTree.parallel(testTree)
    }
  }

  private def generateRandomTree(depth: Int, lowerNumberBound: Int, upperNumberBound: Int): BT[Int] = {
    def generateRandomTreeHelper(actualDepth: Int): BT[Int] = {
      if (actualDepth == depth) Empty
      else Node(Random.between(lowerNumberBound, upperNumberBound + 1), generateRandomTreeHelper(actualDepth + 1), generateRandomTreeHelper(actualDepth + 1))
    }

    if (lowerNumberBound <= 0 || upperNumberBound <= 0) throw new IllegalArgumentException("Non positive numbers are not allowed")
    else if (lowerNumberBound > upperNumberBound) throw new IllegalArgumentException("Lower bound cannot be greater than upper")
    else if (depth < 0) throw new IllegalArgumentException("Depth is not allowed to be negative")
    else if (depth == 0) Empty
    else if (depth == 1) Node(Random.between(lowerNumberBound, upperNumberBound + 1), Empty, Empty)
    else generateRandomTreeHelper(0)
  }

}
