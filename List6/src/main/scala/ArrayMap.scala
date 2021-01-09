import Par.parallel
import scala.util.Random

// Parallelization of Array map higher order function

object ArrayMap {
  private def mapRange[A, B](in: Array[A], out: Array[B])
               (f: A => B)
               (left: Int, right: Int): Unit = {
    for (i <- left until right) {
      out(i) = f(in(i))
    }
  }

  private def mapRangeParallel[A, B](in: Array[A], out: Array[B])
                            (f: A => B)
                            (left: Int, right: Int): Unit = {

    // Separators
    val sep = ((right - left) / 4, (right - left) / 2, 3 * (right - left) / 4)

    // 4 computations in parallel
    parallel(
      mapRange(in, out)(f)(0, sep._1),
      mapRange(in, out)(f)(sep._1 + 1, sep._2),
      mapRange(in, out)(f)(sep._2 + 1, sep._3),
      mapRange(in, out)(f)(sep._3 + 1, in.length))
  }

  def map[A, B](in: Array[A], out: Array[B])(f: A => B)(parallel: Boolean = false): Unit = {
    if (parallel) {
      mapRangeParallel(in, out)(f)(0, in.length)
    } else {
      mapRange(in, out)(f)(0, in.length)
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
    Random.setSeed(0)
    var i = 0
    while (i < xs.length) {
      xs(i) = Random.nextInt(500000)
      i += 1
    }
  }

  def benchmark(): Unit = {
    val length = 1000000
    val in = Array.fill(length)(0)
    val out = Array.fill(length)(1)

    val func = (x: Int) => x * x
    val func1 = (x: Int) => (Math.cosh(x) + Math.exp(x)).intValue()

    val seqtime = standardConfig setUp {
      _ => initialize(in)
    } measure {
      map(in, out)(func)(parallel = false)
    }


    val partime = standardConfig setUp {
      _ => initialize(in)
    } measure {
      map(in, out)(func)(parallel = true)
    }

    println(s"\tsequential time: $seqtime")
    println(s"\tparallel time: $partime")
  }

  /*
  numThreads = 4

  // Size 1 000 000, Range: 500 000, func = x^2
  sequential time: 8.4351265 ms
	parallel time: 4.104370033333334 ms

	// Size 10 000 000, Range: 500 000, func = x^2
  sequential time: 91.71298653333334 ms
	parallel time: 40.72006663333333 ms

	-------------------------------------------------

  // Size 1 000 000, Range: 500 000, func = (Math.cosh(x) + Math.exp(x)).intValue()
  sequential time: 28.846920000000008 ms
	parallel time: 11.722153433333329 ms

	// Size 10 000 000, Range: 500 000, func = (Math.cosh(x) + Math.exp(x)).intValue()
  sequential time: 316.2063998333333 ms
	parallel time: 92.18210326666666 ms
   */
}
