
object l6 {

  def compareNanoTime[R](task1: => R)(task2: => R): (R,R) = {
    val timeStart1 = System.nanoTime()
    val result1 = task1
    val timeEnd1 = System.nanoTime()
    val time1 = timeEnd1 - timeStart1
    println("Time NotParallel: "+ time1 + " nanoSeconds")

    val timeStart2 = System.nanoTime()
    val result2 = task2
    val timeEnd2 = System.nanoTime()
    val time2 = timeEnd2 - timeStart2
    println("Time Parallel:    "+ time2 + " nanoSeconds")
    println("Ratio: "+((time1.toDouble/time2.toDouble*1000.0).floor)/1000.0)
    println("Differ: "+(time1-time2)+ " nanoSeconds")
    (result1, result2)
  }
}


