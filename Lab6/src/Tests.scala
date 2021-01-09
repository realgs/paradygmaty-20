import Utils._
import QuickSort._

object Tests {

  def main(args: Array[String]): Unit = {

    allFunctionsTests()
    quickSortTimeTest()
  }

  def allFunctionsTests(): Unit = ???

  def quickSortTimeTest(): Unit = {

    println("QUICKSORT TEST\n")

    quickSortTimeTest(10)
    quickSortTimeTest(20)
    quickSortTimeTest(30)
  }

  def quickSortTimeTest(n: Int): Unit = {

    val testArray = generateRandomIntArray(n, -1000, 5000)

    println(testArray.mkString("Array(", ", ", ")"))
    calculateTime("Quicksort sequential", quickSortSequential(testArray))
    calculateTime("Quicksort parallel", quickSortParallel(testArray.clone()))
  }

}
