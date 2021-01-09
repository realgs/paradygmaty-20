package Tests

import Tasks.Quicksort

import scala.util.Random

object QuicksortTest {

  def arrayTest(): Unit = {
    timeTest(10, 1000)
    timeTest(10, 100000)
    timeTest(10, 10000000)
  }

  def timeTest(times: Int, size: Int): Unit = {
    var time = (0.toLong, 0.toLong)
    var currentTime = (0.toLong, 0.toLong)
    for (_ <- 1 to times) {
      val array = Array.fill(size)(Random.nextInt(10000))
      currentTime = Utils.measureTime(Quicksort.quicksort(array.clone()), Quicksort.quicksortParallel(array.clone()))
      time = (time._1 + currentTime._1, time._2 + currentTime._2)
    }

    println("**********************************************************")
    println(times + " different runs of quicksort on arrays size: " + size)
    println("Average time for sequential quicksort: " + (currentTime._1 / times))
    println("Average time for parallel quicksort:   " + (currentTime._2 / times))
    println("**********************************************************\n")
  }
}
