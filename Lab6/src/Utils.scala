object Utils {

  def timeMillis[A](block: => A): Unit = {
    val t0 = System.currentTimeMillis()
    block
    val t1 = System.currentTimeMillis()
    println("Total time: " + (t1 - t0) + "ms")
  }

  def timeNanos[A](block: => A): Unit = {
    val t0 = System.nanoTime()
    block
    val t1 = System.nanoTime()
    println("Total time: " + (t1 - t0) + "ns")
  }

}
