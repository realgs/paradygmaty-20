package Tests

import scala.util.Random
import main.Sorts._

object SortTest {

  def runTest(): Unit = {
    test1()
    test2()
  }

  def test1(): Unit = {
    println("1.Quicksort test")
    val array = Array(-1, -2, 1, 150, 1, 30, 10, -1)
    val desirableArray = Array(-2, -1, -1, 1, 1, 10, 30, 150)
    val arraySequential = array.clone()
    val arrayFuture = array.clone()
    val arrayParallel = array.clone()
    quicksort(arraySequential)
    quicksortFuture(arrayFuture)
    quicksortParallel(arrayParallel)
    println(arraySequential sameElements desirableArray)
    println(arrayFuture sameElements desirableArray)
    println(arrayParallel sameElements desirableArray)
  }

  def test2(): Unit ={
    println("2.Quicksort test")
      val array = Array.fill(1000)(Random.nextInt(1000))
      val arraySequential = array.clone()
      val arrayFuture = array.clone()
      val arrayParallel = array.clone()
      quicksort(arraySequential)
      quicksortFuture(arrayFuture)
      quicksortParallel(arrayParallel)
      val desirableArray = array.sorted
      println(arraySequential sameElements desirableArray)
      println(arrayFuture sameElements desirableArray)
      println(arrayParallel sameElements desirableArray)
  }



}
