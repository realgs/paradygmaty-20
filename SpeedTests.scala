package lab6

import sequence._
import scala.util.Random

object SpeedTests {
  private def createMatrix(rows: Int, cols: Int): Matrix = {
    val matrix = new Matrix(rows, cols)
    for (r <- 0 until rows) {
      for (c <- 0 until cols) {
        matrix.matrix(r)(c) = Random.nextInt()
      }
    }
    matrix
  }

  def matrixSpeedTest(size: Int): Unit = {
    println(" Matrix size "+size+"x"+size)
    val matrixA = createMatrix(size, size)
    val matrixB = createMatrix(size, size)
    val seqTime = algorithmTime(multiplyAxB(matrixA, matrixB))
    val parTime = algorithmTime(parallel.multiplyAxB(matrixA, matrixB))
    println("   seqTime " + seqTime)
    println("   parTime " + parTime)
    println("   differ " + (seqTime - parTime)+" ratio "+seqTime/parTime)
  }
  private def createLists(sizeXs: Int): List[Int]={
      var xs = List[Int]()
      for(i<-0 until sizeXs) {
          xs = Random.nextInt(i+1) :: xs
      }
      xs
  }
  private def createArray(sizeXs: Int): Array[Int]={
      val arr = new Array[Int](sizeXs)
      for(i<-arr.indices){
          arr(i) = Random.nextInt()
      }
      arr
  }
  def patternsSpeedTest(sizeXs: Int,sizePat: Int): Unit ={
    println(" Array size "+sizeXs+" Number of patterns "+sizePat)
    val xs = createArray(sizeXs)
    val pat = createLists(sizePat)
    val seqTime = algorithmTime(findPattern(xs,pat))
    val parTime = algorithmTime(parallel.findPattern(xs,pat))
    println("   seqTime " + seqTime)
    println("   parTime " + parTime)
    println("   differ " + (seqTime - parTime)+" ratio "+seqTime/parTime)
  }
  def quicksortSpeedTest(arraySize: Int): Unit=
  {
      println(" Quicksort Array Size "+ arraySize)
      val arr = createArray(arraySize)
      val arr2 = arr.clone()
      val seqTime = algorithmTime(quicksort(arr))
      val parTime = algorithmTime(parallel.quicksort(arr2))
      println("   seqTime " + seqTime)
      println("   parTime " + parTime)
      println("   differ " + (seqTime - parTime)+" ratio "+seqTime/parTime)
  }
  private def algorithmTime[A](task: => A): Double = {
    val startTime = System.currentTimeMillis()
    task
    System.currentTimeMillis() - startTime
  }
  def runSpeedTestsMatrix(): Unit={
      println("Matrix Speed Tests")
      matrixSpeedTest(100)
      matrixSpeedTest(500)
      matrixSpeedTest(1000)
      println()
  }
  def runSpeedTestsPattern(): Unit={
    println("Find Pattern Speed Tests ")
    patternsSpeedTest(10000,100)
    patternsSpeedTest(1000000,100)
    patternsSpeedTest(10000,300)
    patternsSpeedTest(1000000,300)
    println()
  }
  def runSpeedTestsQuickSort(): Unit={
    println("QuickSort Speed Tests ")
    quicksortSpeedTest(100)
    quicksortSpeedTest(100000)
    quicksortSpeedTest(10000000)
    println()
  }

}
