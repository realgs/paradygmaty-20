package Tests

object Utils {

  def measureTime[T](task1: => T, task2: => T): (Long, Long) = {
    var startTime = System.nanoTime()
    task1
    val time1 = System.nanoTime() - startTime
    startTime = System.nanoTime()
    task2
    val time2 = System.nanoTime() - startTime
    (time1, time2)
  }
}
