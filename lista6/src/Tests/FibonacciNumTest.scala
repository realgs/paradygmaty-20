package Tests

import Tasks.FibonacciNum

object FibonacciNumTest {

  def fibTest(): Unit = {
    timeTest(10, 10)
    timeTest(10, 30)
    timeTest(10, 43)
  }

  def timeTest(times: Int, number: Int): Unit = {
    var time = (0.toLong, 0.toLong)
    var currentTime = (0.toLong, 0.toLong)
    for (_ <- 1 to times) {
      currentTime = Utils.measureTime(FibonacciNum.fib(number), FibonacciNum.fibParallel(number))
      time = (time._1 + currentTime._1, time._2 + currentTime._2)
    }

    println("**********************************************************")
    println(times + " different runs of fibonacci number generator, number: " + number)
    println("Average time for sequential fibonacci: " + (currentTime._1 / times))
    println("Average time for parallel fibonacci:   " + (currentTime._2 / times))
    println("**********************************************************\n")
  }
}
