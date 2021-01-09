import Fibonacci._
import Sorts._
import Trees._
import Utils._
import Computations._

import scala.util.Random

object Tests {

  def main(args: Array[String]): Unit = {
    quickSortTimeTests()
    removeRepetitionTimeTest()
    fibonacciTimeTest()
    arrayMaxValueTimeTest()
  }

  def quickSortTimeTests(): Unit = {
    println("QuickSort time test\n")
    val smallArray = Array.fill(1000)(Random.between(0, 100000))
    val middleArray = Array.fill(100000)(Random.between(0, 100000))
    val bigArray = Array.fill(10000000)(Random.between(0, 100000))


    println("QuickSort 1000 elements ")
    print("Sequential: ")
    timeNanos(quicksort(smallArray.clone()))
    print("Future: ")
    timeNanos(quicksortFuture(smallArray.clone()))
    print("Parallel: ")
    timeNanos(quicksortParallel(smallArray.clone()))

    println("QuickSort 100000 elements ")
    print("Sequential: ")
    timeNanos(quicksort(middleArray.clone()))
    print("Future: ")
    timeNanos(quicksortFuture(middleArray.clone()))
    print("Parallel: ")
    timeNanos(quicksortParallel(middleArray.clone()))

    println("QuickSort 100000000 elements ")
    print("Sequential: ")
    timeMillis(quicksort(bigArray.clone()))
    print("Future: ")
    timeMillis(quicksortFuture(bigArray.clone()))
    print("Parallel: ")
    timeMillis(quicksortParallel(bigArray.clone()))
  }

  def removeRepetitionTimeTest(): Unit = {
    println("------------------------------\nremoving repetition in tree time test")
    val (smallTree1, smallTree2) = (fullTree(5, 0, 100), fullTree(5, 0, 100))
    val (middleTree1, middleTree2) = (fullTree(15, 0, 100), fullTree(15, 0, 100))
    val (bigTree1, bigTree2) = (fullTree(22, 0, 100), fullTree(22, 0, 100))

    println("\nTree of height 5 ")
    print("Sequential: ")
    timeNanos(removeRepetition(smallTree1, smallTree2))
    print("Future: ")
    timeNanos(removeRepetitionFuture(smallTree1, smallTree2))
    print("Parallel: ")
    timeNanos(removeRepetitionParallel(smallTree1, smallTree2))

    println("\nTree of height 15 ")
    print("Sequential: ")
    timeNanos(removeRepetition(middleTree1, middleTree2))
    print("Future: ")
    timeNanos(removeRepetitionFuture(middleTree1, middleTree2))
    print("Parallel: ")
    timeNanos(removeRepetitionParallel(middleTree1, middleTree2))

    println("\nTree of height 22 ")
    print("Sequential: ")
    timeNanos(removeRepetition(bigTree1, bigTree2))
    print("Future: ")
    timeNanos(removeRepetitionFuture(bigTree1, bigTree2))
    print("Parallel: ")
    timeNanos(removeRepetitionParallel(bigTree1, bigTree2))

  }

  def fibonacciTimeTest(): Unit = {
    println("------------------------------\nFibonacci algorithm time test")

    println("\n10th fibonacci number ")
    print("Sequential: ")
    timeNanos(fibonacci(10))
    print("Future: ")
    timeNanos(fibonacciFuture(10))
    print("Parallel: ")
    timeNanos(fibonacciParallel(10))

    println("\n20th fibonacci number ")
    print("Sequential: ")
    timeNanos(fibonacci(25))
    print("Future: ")
    timeNanos(fibonacciFuture(25))
    print("Parallel: ")
    timeNanos(fibonacciParallel(25))

    println("\n40th fibonacci number ")
    print("Sequential: ")
    timeNanos(fibonacci(40))
    print("Future: ")
    timeNanos(fibonacciFuture(40))
    print("Parallel: ")
    timeNanos(fibonacciParallel(40))

  }

  def arrayMaxValueTimeTest(): Unit = {
    println("------------------------------\nFinding max value in array time test")

    val middleArray = Array.fill(10000)(Random.between(-100, 100))
    val bigArray = Array.fill(100000)(Random.between(-100, 100))

    println("\nArray of 10000 elements ")
    print("Sequential: ")
    timeNanos(arrayMaxValue(middleArray))
    print("Future: ")
    timeNanos(arrayMaxValueFuture(middleArray))
    print("Parallel: ")
    timeNanos(arrayMaxValueParallel(middleArray))

    println("\nArray of 100000 elements ")
    print("Sequential: ")
    timeNanos(arrayMaxValue(bigArray))
    print("Future: ")
    timeNanos(arrayMaxValueFuture(bigArray))
    print("Parallel: ")
    timeNanos(arrayMaxValueParallel(bigArray))

  }

}
