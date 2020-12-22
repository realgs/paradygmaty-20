import QuickSort._
import Utilities._

object TimePerformanceTest extends App {
////////////////////////////// Time Tests //////////////////////////////////////
  quickSortTimeTests()



////////////////////////////// Test Methods /////////////////////////////////

  def quickSortTimeTests(): Unit = {
    val tenThousandElements = Array.fill(10000)(random.nextInt(1000000)-500000)
    val hundredThousandElements = Array.fill(100000)(random.nextInt(1000000)-500000)
    val millionElements = Array.fill(1000000)(random.nextInt(1000000)-500000)
    val tenMillionElements = Array.fill(10000000)(random.nextInt(1000000)-500000)

    println("*********************************** QuickSort array with 10000 elements ***********************************")
    print("Normal quick sort: ")
    timeMeasureMilliSeconds(quickSort(tenThousandElements.clone()))
    print("Future quick sort: ")
    timeMeasureMilliSeconds(quickSortFuture(tenThousandElements.clone()))
    print("Parallel quick sort: ")
    timeMeasureMilliSeconds(quickSortParallel(tenThousandElements.clone()))

    println("*********************************** QuickSort array with 100000 elements ***********************************")
    print("Normal quick sort: ")
    timeMeasureMilliSeconds(quickSort(hundredThousandElements.clone()))
    print("Future quick sort: ")
    timeMeasureMilliSeconds(quickSortParallel(hundredThousandElements.clone()))
    print("Parallel quick sort: ")
    timeMeasureMilliSeconds(quickSortParallel(hundredThousandElements.clone()))

    println("*********************************** QuickSort array with 1000000 elements ***********************************")
    print("Normal quick sort: ")
    timeMeasureMilliSeconds(quickSort(millionElements.clone()))
    print("Future quick sort: ")
    timeMeasureMilliSeconds(quickSortParallel(millionElements.clone()))
    print("Parallel quick sort: ")
    timeMeasureMilliSeconds(quickSortParallel(millionElements.clone()))

    println("*********************************** QuickSort array with 10000000 elements ***********************************")
    print("Normal quick sort: ")
    timeMeasureMilliSeconds(quickSort(tenMillionElements.clone()))
    print("Future quick sort: ")
    timeMeasureMilliSeconds(quickSortParallel(tenMillionElements.clone()))
    print("Parallel quick sort: ")
    timeMeasureMilliSeconds(quickSortParallel(tenMillionElements.clone()))
  }





}


