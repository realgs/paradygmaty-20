// Konrad Karanowski
import java.util.concurrent.ThreadLocalRandom
import scala.collection.parallel.CollectionConverters._

/*
  Calculating definite integral of one-parameter function using monte carlo method.
  ThreadLocalRandom is used because of its better performance in multi-threaded tasks.
  In order to parallelize the calculations, I used the parallel for-loop.
  N jobs determine level of parallelization (1 means no parallelization).
 */

package object MonteCarloIntegration
{
  private def estimateIntegral(f: Double => Double, a: Double, b: Double, numSamples: Int): Double =
  {
    @scala.annotation.tailrec
    def estimateIntegralRec(result: Double, samplesRemain: Int): Double =
    {
      if (samplesRemain > 0)
      {
        val x = a + (b - a) * ThreadLocalRandom.current().nextDouble()
        estimateIntegralRec(result + f(x), samplesRemain - 1)
      }
      else result
    }
    estimateIntegralRec(0, numSamples) / numSamples * (b - a)
  }

  def estimateIntegralSequential(f: Double => Double, a: Double, b: Double, numSamples: Int): Double =
  {
    estimateIntegral(f, a, b, numSamples)
  }

  def estimateIntegralParallel(f: Double => Double, a: Double, b: Double, numSamples: Int): Double =
  {
    val nJobs = Runtime.getRuntime.availableProcessors()
    val split = numSamples / nJobs
    (0 until nJobs).toList.par.map {_ => estimateIntegral(f, a, b, split)}.sum / numSamples
  }
}
