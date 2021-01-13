import org.scalameter.{Key, Measurer, Quantity, Warmer, config}

object UsageMeasurer {
  def measureTime[A, B](taskA: => A, taskB: => B = (), benchmarkRuns: Int, showDetails: Boolean): Quantity[Double] = {
    if(benchmarkRuns <= 0) throw new Exception("Can't run negative number of times")
    else config(Key.exec.benchRuns -> benchmarkRuns, Key.verbose -> showDetails) withWarmer {
      new Warmer.Default
    } withMeasurer {
      new Measurer.IgnoringGC
    } measure { taskA; taskB }
  }

  def measureTime2[A, B, C, D](taskA: => A, taskB: => B = (), taskC: => C = (), taskD: => D = (), benchmarkRuns: Int, showDetails: Boolean): Quantity[Double] = {
    if(benchmarkRuns <= 0) throw new Exception("Can't run negative number of times")
    else config(Key.exec.benchRuns -> benchmarkRuns, Key.verbose -> showDetails) withWarmer {
      new Warmer.Default
    } withMeasurer {
      new Measurer.IgnoringGC
    } measure { taskA; taskB; taskC; taskD }
  }

  def measureMemoryUsage[A, B](taskA: => A, taskB: => B = (), benchmarkRuns: Int, showDetails: Boolean): Quantity[Double] = {
    if(benchmarkRuns <= 0) throw new Exception("Can't run negative number of times")
    else config(Key.exec.benchRuns -> benchmarkRuns, Key.verbose -> showDetails) withMeasurer {
      new Measurer.MemoryFootprint
    } measure { taskA; taskB }
  }

  def measureMemoryUsage2[A, B, C, D](taskA: => A, taskB: => B = (), taskC: => C = (), taskD: => D = (), benchmarkRuns: Int, showDetails: Boolean): Quantity[Double] = {
    if(benchmarkRuns <= 0) throw new Exception("Can't run negative number of times")
    else config(Key.exec.benchRuns -> benchmarkRuns, Key.verbose -> showDetails) withMeasurer {
      new Measurer.MemoryFootprint
    } measure { taskA; taskB; taskC; taskD }
  }
}

