// Adrian Chłopowiec
package tests

import algorithms.{ClosestPair, ClosestPairParallel, Point}
import org.scalameter.measure

/*
  The last problem is the problem of finding the smallest distance between a pair of points in a given array. Algorithms
  time complexity is O(n*log^2(n)). Similarly to other problems, parallel algorithm is faster for larger arrays.

  For example:
  n - number of Points

  n = 120
  Parallel time: 2274.2716 ms
  Seq time: 3810.5653 ms

  n = 10
  Parallel time: 49.829 ms
  Seq time: 10.0522 ms
 */
object TestClosestPair
{
  def main(args: Array[String]): Unit =
  {
    val random = scala.util.Random

    val points = Array.fill(10)(new Point(random.nextDouble() * 1000, random.nextDouble() * 1000))
    val seqTime = measure
    {
      ClosestPair.closestDistance(points)
    }

    val parTime = measure
    {
      ClosestPairParallel.closestDistance(points)
    }

    println("Closest Pair")
    println("Parallel time: " + parTime)
    println("Seq time: " + seqTime)
  }
}
