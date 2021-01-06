// Adrian Chłopowiec
package algorithms

import java.util.concurrent.ThreadLocalRandom

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

object Quicksort
{
  def quicksort (xs: Array[Int]): Array[Int] = {
    if(xs.length <= 1) xs
    else {
      val pivot = xs(ThreadLocalRandom.current().nextInt(xs.length))
      Array.concat(
        quicksort(xs filter pivot.>),
        xs filter pivot.==,
        quicksort(xs filter pivot.<)
      )
    }
  }
}

object QuicksortParallel
{
  private def parallelDepth(): Int =
    {
      val processors = Runtime.getRuntime.availableProcessors()
      val binary = processors.toBinaryString
      binary.length - binary.indexOf(1)
    }

  private def quicksortPar(arr: Array[Int], depth: Int): Array[Int] =
  {
    if(depth == 0)
      quicksortNorm(arr)
    else
      {
        if(arr.length <= 1) arr
        else
          {
            val pivot = arr(ThreadLocalRandom.current().nextInt(arr.length))
            val leftFuture = Future {quicksortPar(arr filter pivot.>, depth - 1)}
            val rightFuture = Future {quicksortPar(arr filter pivot.<, depth - 1)}

            val middleElem = arr filter pivot.==
            val leftElems = Await.result(leftFuture, 600.seconds)
            val rightElems = Await.result(rightFuture, 600.seconds)

            Array.concat(leftElems, middleElem, rightElems)
          }
      }
  }

  private def quicksortNorm (arr: Array[Int]): Array[Int] = {
    if(arr.length <= 1) arr
    else {
      val pivot = arr(ThreadLocalRandom.current().nextInt(arr.length))
      Array.concat(
        quicksortNorm(arr filter pivot.>),
        arr filter pivot.==,
        quicksortNorm(arr filter pivot.<)
      )
    }
  }

  def quicksort(arr: Array[Int]): Array[Int] =
  {
    quicksortPar(arr, parallelDepth())
  }
}
