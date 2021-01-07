import QuickSort.QuickSort._
import Utilities._
import MergeSort.MergeSort._
import ClosestPairOfPoints.ClosestPairOfPoints._
import ClosestPairOfPoints.Point
import Martix.Matrix

object TimePerformanceTest extends App {
////////////////////////////// Time Tests //////////////////////////////////////
  quickSortTimeTests()
  mergeSortTimeTests()
  closestPairOfPointsTest()
  matricesMultiplicationTests()


////////////////////////////// Test Methods /////////////////////////////////

  def quickSortTimeTests(): Unit = {
    val tenThousandElements = Array.fill(10000)(random.nextInt(1000000)-500000)
    val hundredThousandElements = Array.fill(100000)(random.nextInt(1000000)-500000)
    val millionElements = Array.fill(1000000)(random.nextInt(1000000)-500000)
    val tenMillionElements = Array.fill(10000000)(random.nextInt(1000000)-500000)


    println("*********************************** QuickSort array with 100000 elements ***********************************")
    print("Normal quick sort: ")
    timeMeasureMilliSeconds(quickSort(hundredThousandElements.clone()))
    print("Future quick sort: ")
    quickSortFuture(hundredThousandElements.clone()) // warm up
    timeMeasureMilliSeconds(quickSortFuture(hundredThousandElements.clone()))
    print("Parallel quick sort: ")
    quickSortParallel(tenThousandElements.clone()) // warm up
    timeMeasureMilliSeconds(quickSortParallel(hundredThousandElements.clone()))

    println("*********************************** QuickSort array with 1000000 elements ***********************************")
    print("Normal quick sort: ")
    timeMeasureMilliSeconds(quickSort(millionElements.clone()))
    print("Future quick sort: ")
    quickSortFuture(hundredThousandElements.clone()) // warm up
    timeMeasureMilliSeconds(quickSortFuture(millionElements.clone()))
    print("Parallel quick sort: ")
    quickSortParallel(tenThousandElements.clone()) // warm up
    timeMeasureMilliSeconds(quickSortParallel(millionElements.clone()))

    println("*********************************** QuickSort array with 10000000 elements ***********************************")
    print("Normal quick sort: ")
    timeMeasureMilliSeconds(quickSort(tenMillionElements.clone()))
    print("Future quick sort: ")
    quickSortFuture(hundredThousandElements.clone()) // warm up
    timeMeasureMilliSeconds(quickSortFuture(tenMillionElements.clone()))
    print("Parallel quick sort: ")
    quickSortParallel(tenThousandElements.clone()) // warm up
    timeMeasureMilliSeconds(quickSortParallel(tenMillionElements.clone()))
  }

  def mergeSortTimeTests(): Unit = {
    val hundredElements = Array.fill(100)(random.nextInt(1000000)-500000)
    val thousandElements = Array.fill(1000)(random.nextInt(1000000)-500000)
    val tenThousandElements = Array.fill(10000)(random.nextInt(1000000)-500000)
    val hundredThousandElements = Array.fill(100000)(random.nextInt(1000000)-500000)


    println("*********************************** MergeSort array with 1000 elements ***********************************")
    print("Normal merge sort: ")
    timeMeasureMilliSeconds(mergeSort(thousandElements.clone()))
    print("Future merge sort: ")
    mergeSortFuture(hundredElements.clone()) //warm up
    timeMeasureMilliSeconds(mergeSortFuture(thousandElements.clone()))
    print("Parallel merge sort: ")
    mergeSortParallel(hundredElements.clone()) //warm up
    timeMeasureMilliSeconds(mergeSortParallel(thousandElements.clone()))

    println("*********************************** MergeSort array with 10000 elements ***********************************")
    print("Normal merge sort: ")
    timeMeasureMilliSeconds(mergeSort(tenThousandElements.clone()))
    print("Future merge sort: ")
    mergeSortFuture(hundredElements.clone()) //warm up
    timeMeasureMilliSeconds(mergeSortFuture(tenThousandElements.clone()))
    print("Parallel merge sort: ")
    mergeSortParallel(hundredElements.clone()) //warm up
    timeMeasureMilliSeconds(mergeSortParallel(tenThousandElements.clone()))

    println("*********************************** MergeSort array with 100000 elements ***********************************")
    print("Normal merge sort: ")
    timeMeasureMilliSeconds(mergeSort(hundredThousandElements.clone()))
    print("Future merge sort: ")
    mergeSortFuture(hundredElements.clone()) //warm up
    timeMeasureMilliSeconds(mergeSortFuture(hundredThousandElements.clone()))
    print("Parallel merge sort: ")
    mergeSortParallel(hundredElements.clone()) //warm up
    timeMeasureMilliSeconds(mergeSortParallel(hundredThousandElements.clone()))
  }

  def closestPairOfPointsTest(): Unit = {
    val thousandElements = new Array[Point](1000)
    val tenThousandElements = new Array[Point](10000)
    val fiftyThousandElements = new Array[Point](50000)

    for(i <- 0 until 1000)
      thousandElements(i) = new Point((random.nextDouble() * 1000) - 500 , (random.nextDouble() * 1000) - 500 )


    for(i <- 0 until 10000)
      tenThousandElements(i) = new Point((random.nextDouble() * 10000) - 5000 , (random.nextDouble() * 10000) - 5000 )

    for(i <- 0 until 50000)
      fiftyThousandElements(i) = new Point((random.nextDouble() * 10000) - 5000 , (random.nextDouble() * 10000) - 5000 )


    println("*********************************** ClosestPairOfPoints array with 1000 elements ***********************************")
    print("Normal closestPairOfPoints: ")
    timeMeasureMilliSeconds(closestPairOfPoints(thousandElements))
    print("Future closestPairOfPoints: ")
    closestPairOfPointsFuture(thousandElements) //warm up
    timeMeasureMilliSeconds(closestPairOfPointsFuture(thousandElements))
    print("Parallel closestPairOfPoints: ")
    closestPairOfPointsParallel(thousandElements) //warm up
    timeMeasureMilliSeconds(closestPairOfPointsParallel(thousandElements))

    println("*********************************** ClosestPairOfPoints array with 10000 elements ***********************************")
    print("Normal closestPairOfPoints: ")
    timeMeasureMilliSeconds(closestPairOfPoints(tenThousandElements))
    print("Future closestPairOfPoints: ")
    closestPairOfPointsFuture(thousandElements) //warm up
    timeMeasureMilliSeconds(closestPairOfPointsFuture(tenThousandElements))
    print("Parallel closestPairOfPoints: ")
    closestPairOfPointsParallel(thousandElements) //warm up
    timeMeasureMilliSeconds(closestPairOfPointsParallel(tenThousandElements))

    println("*********************************** ClosestPairOfPoints array with 50000 elements ***********************************")
    print("Normal closestPairOfPoints: ")
    timeMeasureMilliSeconds(closestPairOfPoints(fiftyThousandElements))
    print("Future closestPairOfPoints: ")
    closestPairOfPointsFuture(thousandElements) //warm up
    timeMeasureMilliSeconds(closestPairOfPointsFuture(fiftyThousandElements))
    print("Parallel closestPairOfPoints: ")
    closestPairOfPointsParallel(thousandElements) //warm up
    timeMeasureMilliSeconds(closestPairOfPointsParallel(fiftyThousandElements))

  }

  def matricesMultiplicationTests(): Unit = {
    val m1 = createMatrixWithRandomIntegers(10, 20)
    val m2 = createMatrixWithRandomIntegers(20, 30)

    val m3 = createMatrixWithRandomIntegers(100, 200)
    val m4 = createMatrixWithRandomIntegers(200, 300)

    val m5 = createMatrixWithRandomIntegers(1000, 2000)
    val m6 = createMatrixWithRandomIntegers(2000, 3000)

    println("*********************************** Matrices Multiplication [10 x 20] * [20 x 30] ***********************************")
    print("Normal multiply matrices: ")
    timeMeasureMilliSeconds(m1.multiply(m2))
    print("Future multiply matrices: ")
    m1.multiplyFuture(m2) //warm up
    timeMeasureMilliSeconds(m1.multiplyFuture(m2))
    print("Parallel multiply matrices: ")
    m1.multiplyParallel(m2) //warm up
    timeMeasureMilliSeconds(m1.multiplyParallel(m2))

    println("*********************************** Matrices Multiplication [100 x 200] * [200 x 300] ***********************************")
    print("Normal multiply matrices: ")
    timeMeasureMilliSeconds(m3.multiply(m4))
    print("Future multiply matrices: ")
    m1.multiplyFuture(m2) //warm up
    timeMeasureMilliSeconds(m3.multiplyFuture(m4))
    print("Parallel multiply matrices: ")
    m1.multiplyParallel(m2) //warm up
    timeMeasureMilliSeconds(m3.multiplyParallel(m4))

    println("*********************************** Matrices Multiplication [1000 x 2000] * [2000 x 3000] ***********************************")
    print("Normal multiply matrices: ")
    timeMeasureMilliSeconds(m5.multiply(m6))
    print("Future multiply matrices: ")
    m1.multiplyFuture(m2) //warm up
    timeMeasureMilliSeconds(m5.multiplyFuture(m6))
    print("Parallel multiply matrices: ")
    m1.multiplyParallel(m2) //warm up
    timeMeasureMilliSeconds(m5.multiplyParallel(m6))
  }



}




