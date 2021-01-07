package benchmarks

object Benchmark {
  def calculateExecutionTime[R](benchmarkName: String)(block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block
    val t1 = System.currentTimeMillis()
    println(s"${benchmarkName} time: ${t1 - t0} ms")
    result
  }

}
