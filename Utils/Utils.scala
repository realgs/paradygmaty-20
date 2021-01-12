package Utils

object Utils {
  def measureExecTime[A](f: => A): Long = {
    val startTime = System.currentTimeMillis()
    f
    val endTime = System.currentTimeMillis()
    endTime - startTime
  }
}
