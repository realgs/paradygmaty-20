import Par.parallel
import scala.util.Random

// Parallelization of Pi estimation using Monte Carlo method

object MonteCarlo {
  // We randomly sample points from 1/4 of a square with unit circle inscribed in it
  // We count the hits which land inside the circle, as we know formula for area of the circle my may deduce pi
  def countHits(numIterations: Int): Int = {
    val randX = new Random
    val randY = new Random

    var hits = 0

    for (i <- 0 until numIterations) {
      val x = randX.nextDouble()
      val y = randY.nextDouble()
      if (x * x + y * y < 1) hits += 1
    }
    hits
  }

  def piSeq(numIter: Int): Double = 4.0 * countHits(numIter) / numIter

  def piPar(numIter: Int): Double = {
    // Count hits using 4 parallel computations, no shared resources used, which allows for great scalability
    val (h1, h2, h3, h4) = parallel(countHits(numIter / 4),
      countHits(numIter / 4), countHits(numIter / 4), countHits(numIter / 4))

    4.0 * (h1 + h2 + h3 + h4) / numIter
  }

  import org.scalameter._

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 60,
    Key.exec.benchRuns -> 30,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def benchmark(): Unit = {
    val numIter = 100000

    val seqtime = standardConfig setUp {
      _ => ()
    } measure {
      piSeq(numIter)
    }


    val partime = standardConfig setUp {
      _ => ()
    } measure {
      piPar(numIter)
    }

    println(s"\tsequential time: $seqtime")
    println(s"\tparallel time: $partime")
  }

  /*
  numThreads = 4

  Number of iterations: 100 000
  sequential time: 3.880720000000001 ms
  parallel time: 1.231849933333333 ms

  Number of iterations: 1 000 000
  sequential time: 39.55257323333333 ms
  parallel time: 10.414533166666667 ms

	Number of iterations: 10 000 000
	sequential time: 427.90652329999995 ms
	parallel time: 108.05349016666665 ms
  */
}
