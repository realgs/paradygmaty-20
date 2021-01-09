/**
 * It is worth to use parallelization in cases of large data sets. It's not efficient if are working on smaller data sets.
 * We should set a threshold to avoid using parallelization if it is not worth it and move these simpler sets to be computed sequentially.
 * Context switching takes time, so a number of used threads shouldn't be bigger than number of cores that our CPU has - ForkJoinPool
 */
object Benchmark {
  def calculateExecutionTime[T](testName: String)(operation: => T): T = {
    val timeStart = System.currentTimeMillis()
    val calculatedOperation = operation
    val timeEnd = System.currentTimeMillis()

    println(s"${testName} took ${timeEnd - timeStart}ms")
    calculatedOperation
  }
}
