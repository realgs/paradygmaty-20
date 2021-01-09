import ParallelMechanism.parallel

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object Computations {
  final val threshold = 2000

  def findMax(arr: Array[Int]): Int = {
    var max = arr(0)
    for (i <- arr.indices) {
      max = Math.max(max, arr(i))
    }
    max
  }

  def arrayMaxValue(arr: Array[Int]): Int = findMax(arr)

  def arrayMaxValueParallel(arr: Array[Int]): Int = {
    val n = arr.length
    if (n == 0) 0
    else {
      val (left, right) = arr.splitAt(n / 2)
      if (n > threshold) {

        val (leftResult, rightResult) = parallel(arrayMaxValueParallel(left), arrayMaxValueParallel(right))
        Math.max(leftResult, rightResult)
      }
      else {
        findMax(arr)
      }
    }
  }

  def arrayMaxValueFuture(arr: Array[Int]): Int = {
    val n = arr.length
    if (n == 0) 0
    else {
      val (left, right) = arr.splitAt(n / 2)
      if (n > threshold) {
        val future1 = Future(arrayMaxValue(left))
        val future2 = Future(arrayMaxValue(right))
        val leftResult = Await.result(future1, 2000.seconds)
        val rightResult = Await.result(future2, 2000.seconds)
        Math.max(leftResult, rightResult)
      }
      else {
        findMax(arr)
      }
    }
  }
}
