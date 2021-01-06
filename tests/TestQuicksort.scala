// Adrian Chłopowiec
package tests

import algorithms.{Quicksort, QuicksortParallel}
import org.scalameter.measure

/*
  The first problem I've chosen is testing if quicksort implemented with parallel programming is faster than the
  sequential one. It turns out that for small arrays of size less than 10^6 sequential algorithm is faster than parallel.
  When arrays reach size of ~10^6 parallel is faster. It's probably because launching threads takes time and it became
  worth it only when the time required for computation raised.

  For example:
  n - samples
  n = 1000000
  Parallel Time: ~800-900 ms
  Seq Time: ~1100-1300 ms

  n = 10000
  Parallel Time: ~90 ms
  Seq Time: ~25 ms
 */
object TestQuicksort
{
  def main(args: Array[String]): Unit =
  {
    val random = scala.util.Random
    val arr = Array.fill(10000)(random.nextInt(10000))
    val parTime = measure
    {
      QuicksortParallel.quicksort(arr)
    }

    val seqTime = measure
    {
      Quicksort.quicksort(arr)
    }

    println("Quicksort")
    println("Parallel time: " + parTime)
    println("Seq time: " + seqTime)
  }


}
