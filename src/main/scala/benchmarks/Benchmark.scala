package benchmarks

object Benchmark {
  def calculateExecutionTime[R](benchmarkName: String)(block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println(s"${benchmarkName} time: ${((t1.toDouble - t0.toDouble) / 1_000_000)} ms")
    result
  }
}
