package benchmarks

import algorithms.Fibonacci
import benchmarks.Benchmark.calculateExecutionTime

/*
  Ratio = Sequential time / Parallel time

  Example results:

  WITHOUT THRESHOLD

  Number: 10
  Sequential time: 1,39 ms
  Parallel time: 3,68 ms
  Ratio: 0,37

  Number: 15
  Sequential time: 1,37 ms
  Parallel time: 5,61 ms
  Ratio: 0,24

  Number: 20
  Sequential time: 2,32 ms
  Parallel time: 12,29 ms
  Ratio: 0,18

  Number: 25
  Sequential time: 2,47 ms
  Parallel time: 57,53 ms
  Ratio: 0,04

  WITH THRESHOLD

  Number: 30
  Sequential time: 6,15 ms
  Parallel time: 4,89 ms
  Ratio: 1,25

  Number: 35
  Sequential time: 49,66 ms
  Parallel time: 15,00 ms
  Ratio: 3,31

  Number: 40
  Sequential time: 479,90 ms
  Parallel time: 90,54 ms
  Ratio: 5,30

  Number: 45
  Sequential time: 5102,13 ms
  Parallel time: 835,25 ms
  Ratio: 6,10

  Number: 50
  Sequential time: 57776.71 ms
  Parallel time: 9530.62 ms
  Ratio: 6,06

  Here, we can see how inefficient the parallel algorithm is without setting the threshold - it's even 15 times slower.
  We can also see how parallelization improves time results when we calculate large Fibonacci numbers. It's even 6 times
  faster than sequential one in my PC, in some situations!
*/

object FibonacciBenchmark {
  private val N = 45

  def run() {
    calculateExecutionTime("FibonacciSequential") {
      Fibonacci.sequentially(N)
    }

    calculateExecutionTime("FibonacciParallel") {
      Fibonacci.parallel(N)
    }
  }
}
