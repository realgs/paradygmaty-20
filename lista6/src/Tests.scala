import scala.util.{Random, Sorting}
//TODO Test performance and correctness of longestCommonPrefix
object Tests {
  def generateIntArray(length: Int):Array[Int] = (for (_ <- 1 to length) yield Random.nextInt(10000)).toArray[Int]

  def generateIntMatrix(height: Int,width: Int):Array[Array[Int]] = (for (_ <- 1 to height) yield generateIntArray(width)).toArray[Array[Int]]

  def cloneIntMatrix(matrix: Array[Array[Int]]):Array[Array[Int]] = {
    val clonedMatrix = Array.ofDim[Int](matrix.length,matrix(0).length)

    for(i <- matrix.indices) clonedMatrix(i) = matrix(i).clone()

    clonedMatrix
  }

  def areEqual(expected: Array[Array[Int]],actual: Array[Array[Int]]):Boolean = {
    for(i <- expected.indices) {
      for(j <- expected(i).indices) {
        if(expected(i)(j) != actual(i)(j)) return false
      }
    }
    true
  }

  def computationTimeMillis[A](task: =>A):Double = {
    val startTime = System.nanoTime()
    task
    (System.nanoTime() - startTime)/1000000.0
  }

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
      val arrayToSort = generateIntArray(numberOfElements)
      sequentialResult = sequentialResult + computationTimeMillis{SequentialProgramming.mergeSort(arrayToSort.clone())}/numberOfTests
      parallelResult = parallelResult + computationTimeMillis{ParallelProgramming.mergeSort(arrayToSort)}/numberOfTests
    }
    val relation = if(sequentialResult > parallelResult) "faster" else "slower"
    val result = if(sequentialResult > parallelResult) sequentialResult/parallelResult.doubleValue() else parallelResult/sequentialResult.doubleValue()
    println(s"For $numberOfTests tests: ")
    println(f"Sequential merge sort average time: $sequentialResult%.2f miliseconds")
    println(f"Parallel merge sort average time: $parallelResult%.2f miliseconds")
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

  def matrixProductTest(): Unit = {
    println("Matrix multiplication test")
    println("Maximum height: 100 && Maximum width: 100")
    matrixProductCorrectnessTest(100,100,100)
    matrixProductSpeedTest(100,100,100)
    println("Maximum height: 250 && Maximum width: 250")
    matrixProductCorrectnessTest(50,250,250)
    matrixProductSpeedTest(50,250,250)
    println("Maximum height: 500 && Maximum width: 500")
    matrixProductCorrectnessTest(25,500,500)
    matrixProductSpeedTest(25,500,500)
    println("Maximum height: 1000 && Maximum width: 1000")
    matrixProductCorrectnessTest(10,1000,1000)
    matrixProductSpeedTest(10,1000,1000)
  }

  def matrixProductSpeedTest(numberOfTests:Int,maximumHeight: Int,maximumWidth: Int): Unit = {
    var sequentialResult = 0.0
    var parallelResult = 0.0
    for(_ <- 1 to numberOfTests) {
      val firstMatrix = generateIntMatrix(Random.nextInt(maximumHeight/2) + maximumHeight/2,(maximumWidth + maximumHeight)/2)
      val secondMatrix = generateIntMatrix((maximumWidth + maximumHeight)/2,Random.nextInt(maximumWidth/2) + maximumWidth/2)
      val firstClonedMatrix = cloneIntMatrix(firstMatrix)
      val secondClonedMatrix = cloneIntMatrix(secondMatrix)
      sequentialResult = sequentialResult + computationTimeMillis {SequentialProgramming.matrixProduct(firstClonedMatrix,secondClonedMatrix)}/numberOfTests
      parallelResult = parallelResult + computationTimeMillis {ParallelProgramming.matrixProduct(firstMatrix,secondMatrix)}/numberOfTests
    }

    val relation = if(sequentialResult > parallelResult) "faster" else "slower"
    val result = if(sequentialResult > parallelResult) sequentialResult/parallelResult.doubleValue() else parallelResult/sequentialResult.doubleValue()
    println(s"For $numberOfTests tests: ")
    println(f"Sequential matrix multiplication average time: $sequentialResult%.2f miliseconds")
    println(f"Parallel matrix multiplication average time: $parallelResult%.2f miliseconds")
    println(f"Parallel matrix multiplication is $result%.2f times $relation than sequential matrix multiplication")
  }

  def matrixProductCorrectnessTest(numberOfTests:Int,maximumHeight: Int,maximumWidth: Int): Unit = {
    for(_ <- 0 to numberOfTests) {
      val commonDimension = Random.nextInt((maximumWidth + maximumHeight)/2) + (maximumWidth + maximumHeight)/2
      val firstMatrix = generateIntMatrix(Random.nextInt(maximumHeight/2) + maximumHeight/2,commonDimension)
      val secondMatrix = generateIntMatrix(commonDimension,Random.nextInt(maximumWidth/2) + maximumWidth/2)
      assert(areEqual(SequentialProgramming.matrixProduct(cloneIntMatrix(firstMatrix),cloneIntMatrix(secondMatrix)),ParallelProgramming.matrixProduct(firstMatrix,secondMatrix)))
    }
  }
}
