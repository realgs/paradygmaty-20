// Konrad Karanowski
package Benchmarks

import org.scalameter.{Key, Warmer, config}
import MonteCarloIntegration.{estimateIntegralSequential, estimateIntegralParallel}

object MonteCarloBenchmarks
{

  private def testAlgorithm(algorithm: (Double => Double, Double, Double, Int) => Double)(
        f: Double => Double,
        a: Double,
        b: Double,
        samples: Int,
        runsPerFunction: Int,
        verbose: Boolean = false
  ): Double =
  {
    val time = config(
      Key.exec.maxWarmupRuns -> runsPerFunction,
      Key.verbose -> verbose,
    ) withWarmer(new Warmer.Default) measure
      {
        algorithm(f, a, b, samples)
      }
    time.value
  }

  private def testRun(f: Double => Double, a: Double, b: Double, samples: Int, runsPerFunction: Int): (Double, Double) =
  {
    (
      testAlgorithm(estimateIntegralSequential)(f, a, b, samples, runsPerFunction),
      testAlgorithm(estimateIntegralParallel)(f, a, b, samples, runsPerFunction)
      )
  }

  def testMonteCarlo(differentRuns: Int, f: Double => Double, a: Double, b: Double, samples: Int, runsPerFunction: Int): Unit =
  {
    var parallelTotalTime = .0
    var sequentialTotalTime = .0
    for (_ <- 0 until differentRuns)
    {
      val (sequentialTime, parallelTime) = testRun(f, a, b, samples, runsPerFunction)
      parallelTotalTime += parallelTime
      sequentialTotalTime += sequentialTime
    }
    println(f"Different runs: $differentRuns, Integrate: $f, from $a to $b, number of samples: $samples, Runs per function: $runsPerFunction")
    println(f"Mean time for the sequential solution: ${sequentialTotalTime / differentRuns}ms")
    println(f"Mean time for the parallel solution: ${parallelTotalTime / differentRuns}ms")
    println(f"Ratio sequential/parallel: ${sequentialTotalTime / parallelTotalTime}")
  }
}
