import benchmarks.{FibonacciBenchmark, QuickSortBenchmark, SumOfFullTreeBenchmark}

/*
  In all algorithms I'm using ForkJoinPool to make computations in all available cores as efficient as possible.
  Size of this pool is by default equal to number of cores in CPU

  My main thoughts about parallelization:
  - it becomes worthy when we work on large data sets
  - we should always consider using some threshold - when the number of data is smaller than the threshold
    problem is solved sequentially, otherwise parallel. It's crucial in recursive problems because when we
    queue very small tasks to different CPU cores it can result in many cache misses which results in longer computations
  - we should create as many threads as the used computer has to avoid unnecessary context switches.
    Additionally, when we create and stop threads we waste some time on it.
*/

object Lista6 extends App {
  QuickSortBenchmark.run()
  println()
  FibonacciBenchmark.run()
  println()
  SumOfFullTreeBenchmark.run()
}
