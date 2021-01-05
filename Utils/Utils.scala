package Utils

object Utils {
  def measureExecTime(f: () => Any): Long = {
    val startTime = System.currentTimeMillis()
    f()
    val endTime = System.currentTimeMillis()
    endTime - startTime
  }
}
