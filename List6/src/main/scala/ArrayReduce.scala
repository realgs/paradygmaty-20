import Par.parallel
import scala.util.Random

// Parallelization of Array reduce higher order function

object ArrayReduce {
  private def reduceSeg[A](arr: Array[A], left: Int, right: Int)(f: (A, A) => A): A = {
    if (right - left < 5) {
      var res = arr(left)
      var i = left + 1

      while (i < right) {
        res = f(res, arr(i))
        i += 1
      }
      res
    } else {
      val mid = left + (right - left) / 2
      val (x, y) = parallel(reduceSeg(arr, left, mid)(f), reduceSeg(arr, mid, right)(f))
      f(x, y)
    }
  }

  // Parallelized
  def reduceParallel[A](arr: Array[A])(f: (A, A) => A): A =
    reduceSeg(arr, 0, arr.length)(f)

  // Sequential
  def reduceSeq[A](arr: Array[A])(f: (A, A) => A): A = {
    var res = arr(0)
    var i = 1

    while (i < arr.length) {
      res = f(res, arr(i))
      i += 1
    }
    res
  }

  // Benchmark

  import org.scalameter._

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 60,
    Key.exec.benchRuns -> 30,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def initialize(xs: Array[Int]) {
    Random.setSeed(3)
    var i = 0
    while (i < xs.length) {
      xs(i) = Random.nextInt(1000)
      i += 1
    }
  }

  def benchmark(): Unit = {
    val length = 1000000
    val arr = Array.fill(length)(0)

    val func = (x: Int, y: Int) => x + y

    val addWithLoad = (x: Int, y: Int) => {
      // Simulating load
      val load = Array.fill(1000)(1)
      load.reverse

      // Return
      x + y
    }


    val seqtime = standardConfig setUp {
      _ => initialize(arr)
    } measure {
      arr.reduce(func)
    }


    val partime = standardConfig setUp {
      _ => initialize(arr)
    } measure {
      reduceParallel(arr)(func)
    }

    println(s"\tsequential time: $seqtime")
    println(s"\tparallel time: $partime")
  }

  /*
  numThreads = 2

  Size: 1 000 000, Range: 1 000, func (x, y) => x + y
  sequential time: 6.826283366666667 ms
  parallel time: 5.1128133 ms

	Size: 10 000 000, Range: 1 000, func (x, y) => x + y
	sequential time: 72.46411996666669 ms
	parallel time: 62.01457349999999 ms

  -------------------------------------
	Improvements are minimal, because summing array elements with reduce requires little cost
	i.e. functions applied in reduce is fast

	Suppose func = (x, y) => {doHardCalculations()}
	array fill and reverse is used to simulate such computationally heavy function)
	-------------------------------------

  Size: 10 000, Range: 1 000, func (x, y) => x + y with load
  sequential time: 18.726529966666668 ms
  parallel time: 7.5637266 ms

	Size: 100 000, Range: 1 000, func (x, y) => x + y with load
	sequential time: 200.60887693333333 ms
	parallel time: 53.949243333333335 ms
  */
}
