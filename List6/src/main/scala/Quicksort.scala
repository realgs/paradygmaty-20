import Par.parallel

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

  private def quickParallel(arr: Array[Int], lo: Int, hi: Int): Unit = {
    if (lo < hi) {
      val (i, j) = partition(arr, lo, hi)
      parallel(quick(arr, lo, j), quick(arr, i, hi))
    }
  }

  def sort(arr: Array[Int], parallel: Boolean = false): Unit = {
    if (parallel) {
      quickParallel(arr, 0, arr.length - 1)
    } else {
      quick(arr, 0, arr.length - 1)
    }
  }

  import org.scalameter._

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 60,
    Key.exec.benchRuns -> 30,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def initialize(xs: Array[Int]) {
    // Random.setSeed(3)
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
  // Size: 10 000, Range: 500 000
  sequential time: 0.9334301 ms ms
	parallel time: 1.1053298666666669 ms

  // Size: 100 000, Range: 500 000
  sequential time: 9.653313133333334 ms
	parallel time: 5.656023300000002 ms

	// Size: 1 000 000, Range: 500 000
  sequential time: 147.69995676666665 ms
	parallel time: 102.62426010000006 ms
   */
}
