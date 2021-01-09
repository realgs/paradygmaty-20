import Utils._
import QuickSort._

object Tests {

  def main(args: Array[String]): Unit = {

    allFunctionsTests()
    quickSortTimeTest()
  }

  def allFunctionsTests(): Unit = {

    assert(quickSortSequential(Array(4, 3, 5, -123, 0, 1, 3, 3, 1000, -5, 3)) sameElements Array(-123, -5, 0, 1, 3, 3, 3, 3, 4, 5, 1000))
    assert(quickSortSequential(Array(1, 1, 1, 1, 1)) sameElements Array(1, 1, 1, 1, 1))
    assert(quickSortSequential(Array(100, 99, 98, 97, 97)) sameElements Array(97, 97, 98, 99, 100))
    assert(quickSortSequential(Array()) sameElements Array.emptyIntArray)

    assert(quickSortParallel(Array(4, 3, 5, -123, 0, 1, 3, 3, 1000, -5, 3)) sameElements Array(-123, -5, 0, 1, 3, 3, 3, 3, 4, 5, 1000))
    assert(quickSortParallel(Array(1, 1, 1, 1, 1)) sameElements Array(1, 1, 1, 1, 1))
    assert(quickSortParallel(Array(100, 99, 98, 97, 97)) sameElements Array(97, 97, 98, 99, 100))
    assert(quickSortParallel(Array()) sameElements Array.emptyIntArray)

  }

  def quickSortTimeTest(): Unit = {

    println("QUICKSORT TEST\n")

    quickSortTimeTest(1000)
    quickSortTimeTest(10000)
    quickSortTimeTest(100000)
    quickSortTimeTest(1000000)
  }

  def quickSortTimeTest(n: Int): Unit = {

    val testArray = generateRandomIntArray(n, -1000, 5000)

    println(s"n = $n")
    calculateTime("Quicksort sequential", quickSortSequential(testArray.clone()))
    calculateTime("Quicksort parallel", quickSortParallel(testArray.clone()))

    println()
  }

}
