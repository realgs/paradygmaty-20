import scala.util.{Random, Sorting}

object Main extends App {
  def generateIntArray(length: Int):Array[Int] = (for (_ <- 1 to length) yield Random.nextInt(10000)).toArray[Int]

  def computationTimeNanos[A](task: =>A):Long = {
    val startTime = System.nanoTime()
    task
    System.nanoTime() - startTime
  }

  /* Przetwarzanie równoległe pozwala osiągnąć spory zysk czasowy,ale tylko w sytuacji gdy zadanie do wykonania równoległego jest odpowiedniej wielkości.
  Dla niewielkiej ilości danych przetwarzanie równoległe może spowolnić obliczenia. */
  def mergeSortTest(): Unit = {
    println("Merge sort test")
    println("1000 elements")
    mergeSortOrderTest(100,1000)
    mergeSortSpeedTest(1000,1000)
    println("10000 elements")
    mergeSortOrderTest(100,10000)
    mergeSortSpeedTest(1000,10000)
    println("100000 elements")
    mergeSortOrderTest(100,100000)
    mergeSortSpeedTest(1000,100000)
    println("1000000 elements")
    mergeSortOrderTest(100,1000000)
    mergeSortSpeedTest(100,1000000)
  }
  def mergeSortSpeedTest(numberOfTests:Int,numberOfElements: Int):Unit = {
    var sequentialResult = 0.0
    var parallelResult = 0.0
    for(_ <- 0 until numberOfTests) {
      sequentialResult = sequentialResult + computationTimeNanos{SequentialProgramming.mergeSort(generateIntArray(numberOfElements))}/numberOfTests.doubleValue()
      parallelResult = parallelResult + computationTimeNanos{ParallelProgramming.mergeSort(generateIntArray(numberOfElements))}/numberOfTests.doubleValue()
    }
    val relation = if(sequentialResult > parallelResult) "faster" else "slower"
    val result = if(sequentialResult > parallelResult) sequentialResult/parallelResult.doubleValue() else parallelResult/sequentialResult.doubleValue()
    println(s"For $numberOfTests tests: ")
    println(f"Sequential merge sort average time: ${sequentialResult/1000000.0}%.2f miliseconds")
    println(f"Parallel merge sort average time: ${parallelResult/1000000.0}%.2f miliseconds")
    println(f"Parallel merge sort is $result%.2f times $relation than sequential merge sort")
  }
  def mergeSortOrderTest(numberOfTests:Int,numberOfElements:Int):Unit = {
    for(_ <- 0 to numberOfTests) {
      val comparisonArray = generateIntArray(numberOfElements)
      val arrayToSortSequentially = comparisonArray.clone()
      val arrayToSortParallel = comparisonArray.clone()
      Sorting.quickSort(comparisonArray)
      SequentialProgramming.mergeSort(arrayToSortSequentially)
      SequentialProgramming.mergeSort(arrayToSortParallel)
      assert(arrayToSortSequentially.sameElements(comparisonArray))
      assert(arrayToSortParallel.sameElements(comparisonArray))
    }

  }

  mergeSortTest()
}
