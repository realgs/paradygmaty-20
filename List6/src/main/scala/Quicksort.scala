import Par.parallel

import scala.util.Random

// Parallelized

object Quicksort {
  private def swap[A](arr: Array[A])(i: Int, j: Int): Unit = {
    val temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
  }

  private def partition(arr: Array[Int], lo: Int, hi: Int): (Int, Int) = {
    var i = lo
    var j = hi
    val pivot = arr((lo + hi) / 2)

    while (i <= j) {
      while (arr(i) < pivot) i += 1
      while (pivot < arr(j)) j -= 1
      if (i <= j) {
        swap(arr)(i, j)
        i += 1
        j -= 1
      }
    }
    (i, j)
  }

  private def quick(arr: Array[Int], lo: Int, hi: Int): Unit = {
    if (lo < hi) {
      val (i, j) = partition(arr, lo, hi)
      quick(arr, lo, j)
      quick(arr, i, hi)
    }
  }

  private def quickParallel(arr: Array[Int], lo: Int, hi: Int)(numThreads: Int = 4): Unit = {
    if (lo < hi) {
      val (i, j) = partition(arr, lo, hi)
      if (numThreads >= 4) {
        parallel(quickParallel(arr, lo, j)(numThreads - 2), quickParallel(arr, i, hi)(numThreads - 2))
      } else {
        parallel(quick(arr, lo, j), quick(arr, i, hi))
      }
    }
  }

  def sort(arr: Array[Int], parallel: Boolean = false, numThreads: Int = 32): Unit = {
    if (parallel) {
      quickParallel(arr, 0, arr.length - 1)(numThreads)
    } else {
      quick(arr, 0, arr.length - 1)
    }
  }

  def sortSeq(arr: Array[Int]): Unit = quick(arr, 0, arr.length - 1)

  import org.scalameter._

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 60,
    Key.exec.benchRuns -> 30,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def initialize(xs: Array[Int]) {
    // Random.setSeed(0)
    // Random.nextInt(500000)
    var i = 0
    while (i < xs.length) {
      xs(i) = i % 100
      i += 1
    }
  }

  def benchmark(): Unit = {
    val length = 10000000
    val xs = Array.fill(length)(0)

    val seqtime = standardConfig setUp {
      _ => initialize(xs)
    } measure {
      sort(xs)
    }


    val partime = standardConfig setUp {
      _ => initialize(xs)
    } measure {
      sort(xs, parallel = true)
    }

    println(s"\tsequential time: $seqtime")
    println(s"\tparallel time: $partime")
  }

  /*
  numThreads = 2

  Size: 10 000, Range: 500 000
  sequential time: 0.9334301 ms ms
  parallel time: 1.1053298666666669 ms

  Size: 100 000, Range: 500 000
  sequential time: 9.653313133333334 ms
  parallel time: 5.656023300000002 ms

  Size: 1 000 000, Range: 500 000
  sequential time: 147.69995676666665 ms
  parallel time: 102.62426010000006 ms

  Size: 10 000 000, xs(i) = i % 100
  sequential time: 795.3976433666668 ms
  parallel time: 736.8553333333334 ms

  -----------------------------------------
  numThreads = 16

  Size: 10 000, xs(i) = i % 100
  sequential time: 0.4677567 ms
  parallel time: 0.35525656666666683 ms

  Size: 100 000, xs(i) = i % 100
  sequential time: 5.090790066666665 ms
  parallel time: 4.110456533333333 ms

  Size: 1 000 000, xs(i) = i % 100
  sequential time: 75.25976663333333 ms
  parallel time: 25.6644831 ms

  Size: 10 000 000, xs(i) = i % 100
  sequential time: 854.402456566667 ms
  parallel time: 321.93192660000005 ms

  -----------------------------------------
  numThreads = 32

  Size: 10 000 000, xs(i) = i % 100
  sequential time: 768.1460468000001 ms
  parallel time: 204.55891336666664 ms
  */

}
