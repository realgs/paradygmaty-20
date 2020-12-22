import QuickSort._
import Utilities._
import MergeSort._

object TimePerformanceTest extends App {
////////////////////////////// Time Tests //////////////////////////////////////
  //quickSortTimeTests()
  mergeSortTimeTests()


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
    timeMeasureMilliSeconds(quickSortFuture(hundredThousandElements.clone()))
    print("Parallel quick sort: ")
    timeMeasureMilliSeconds(quickSortParallel(hundredThousandElements.clone()))

    println("*********************************** QuickSort array with 1000000 elements ***********************************")
    print("Normal quick sort: ")
    timeMeasureMilliSeconds(quickSort(millionElements.clone()))
    print("Future quick sort: ")
    timeMeasureMilliSeconds(quickSortFuture(millionElements.clone()))
    print("Parallel quick sort: ")
    timeMeasureMilliSeconds(quickSortParallel(millionElements.clone()))

    println("*********************************** QuickSort array with 10000000 elements ***********************************")
    print("Normal quick sort: ")
    timeMeasureMilliSeconds(quickSort(tenMillionElements.clone()))
    print("Future quick sort: ")
    timeMeasureMilliSeconds(quickSortFuture(tenMillionElements.clone()))
    print("Parallel quick sort: ")
    timeMeasureMilliSeconds(quickSortParallel(tenMillionElements.clone()))
  }

  def mergeSortTimeTests(): Unit = {
    val hundredElements = Array.fill(100)(random.nextInt(1000000)-500000)
    val thousandElements = Array.fill(1000)(random.nextInt(1000000)-500000)
    val tenThousandElements = Array.fill(10000)(random.nextInt(1000000)-500000)
    val hundredThousandElements = Array.fill(100000)(random.nextInt(1000000)-500000)

    println("*********************************** MergeSort array with 100 elements ***********************************")
    print("Normal merge sort: ")
    timeMeasureMilliSeconds(mergeSort(hundredElements.clone()))
    print("Future merge sort: ")
    timeMeasureMilliSeconds(mergeSortFuture(hundredElements.clone()))
    print("Parallel merge sort: ")
    timeMeasureMilliSeconds(mergeSortParallel(hundredElements.clone()))

    println("*********************************** MergeSort array with 1000 elements ***********************************")
    print("Normal merge sort: ")
    timeMeasureMilliSeconds(mergeSort(thousandElements.clone()))
    print("Future merge sort: ")
    timeMeasureMilliSeconds(mergeSortFuture(thousandElements.clone()))
    print("Parallel merge sort: ")
    timeMeasureMilliSeconds(mergeSortParallel(thousandElements.clone()))

    println("*********************************** MergeSort array with 10000 elements ***********************************")
    print("Normal merge sort: ")
    timeMeasureMilliSeconds(mergeSort(tenThousandElements.clone()))
    print("Future merge sort: ")
    timeMeasureMilliSeconds(mergeSortFuture(tenThousandElements.clone()))
    print("Parallel merge sort: ")
    timeMeasureMilliSeconds(mergeSortParallel(tenThousandElements.clone()))

    println("*********************************** MergeSort array with 100000 elements ***********************************")
    print("Normal merge sort: ")
    timeMeasureMilliSeconds(mergeSort(hundredThousandElements.clone()))
    print("Future merge sort: ")
    timeMeasureMilliSeconds(mergeSortFuture(hundredThousandElements.clone()))
    print("Parallel merge sort: ")
    timeMeasureMilliSeconds(mergeSortParallel(hundredThousandElements.clone()))
  }





}


