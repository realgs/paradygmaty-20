package Tests

import Algorithms.{Fibonacci, Matrix, QuickSort}
import Utils.Utils

import scala.util.Random

object TimeTests {
  def main(args: Array[String]): Unit = {
    fibonacciSpeedTest()
    quickSortSpeedTest()
    matrixMultiplicationSpeedTest()
  }

  def fibonacciSpeedTest() = {
    val processors = Runtime.getRuntime.availableProcessors()
    println("--------------------------------- Fibonacci Time Performance ---------------------------------------")
    println("1. Find 10 Fibonacci Number: ")
    printf("   a) Sequential: %d ms", Utils.measureExecTime(Fibonacci.findFibonacci(10)))
    printf("   b) Tail recursion: %d ms", Utils.measureExecTime(Fibonacci.findFibonacciIter(10)))
    printf("   c) Parallel: %d ms %n", Utils.measureExecTime(Fibonacci.findFibonacciParallel(10, processors/2 - 1)))
    println("2. Find 25 Fibonacci Number: ")
    printf("   a) Sequential: %d ms", Utils.measureExecTime(Fibonacci.findFibonacci(25)))
    printf("   b) Tail recursion: %d ms", Utils.measureExecTime(Fibonacci.findFibonacciIter(25)))
    printf("   c) Parallel: %d ms %n", Utils.measureExecTime(Fibonacci.findFibonacciParallel(25, processors/2 - 1)))
    println("3. Find 40 Fibonacci Number: ")
    printf("   a) Sequential: %d ms", Utils.measureExecTime(Fibonacci.findFibonacci(40)))
    printf("   b) Tail recursion: %d ms", Utils.measureExecTime(Fibonacci.findFibonacciIter(40)))
    printf("   c) Parallel: %d ms %n", Utils.measureExecTime(Fibonacci.findFibonacciParallel(40, processors/2 - 1)))
  }

  def quickSortSpeedTest() = {
    val arrayWith100Elem = Array.fill(100)(Random.between(0, 1000))
    val arrayWith10000Elem = Array.fill(10000)(Random.between(0, 1000)) // 10 000
    val arrayWith100000Elem = Array.fill(100000)(Random.between(0, 1000)) // 100 000
    val arrayWith1000000Elem = Array.fill(1000000)(Random.between(0, 1000)) // 1 000 000
    val arrayWith10000000Elem = Array.fill(10000000)(Random.between(0, 1000)) // 10 000 000

    println("--------------------------------- QuickSort Time Performance ---------------------------------------")
    println("1. Sort array with 100 elements:")
    printf("   a) Sequential: %d ms", Utils.measureExecTime(QuickSort.quickSort(arrayWith100Elem.clone())))
    printf("   b) Parallel: %d ms %n", Utils.measureExecTime(QuickSort.quickSortParallel(arrayWith100Elem.clone())))
    println("2. Sort array with 10 000 elements:")
    printf("   a) Sequential: %d ms", Utils.measureExecTime(QuickSort.quickSort(arrayWith10000Elem.clone())))
    printf("   b) Parallel: %d ms %n", Utils.measureExecTime(QuickSort.quickSortParallel(arrayWith10000Elem.clone())))
    println("3. Sort array with 100 000 elements:")
    printf("   a) Sequential: %d ms", Utils.measureExecTime(QuickSort.quickSort(arrayWith100000Elem.clone())))
    printf("   b) Parallel: %d ms %n", Utils.measureExecTime(QuickSort.quickSortParallel(arrayWith100000Elem.clone())))
    println("4. Sort array with 1 000 000 elements:")
    printf("   a) Sequential: %d ms", Utils.measureExecTime(QuickSort.quickSort(arrayWith1000000Elem.clone())))
    printf("   b) Parallel: %d ms %n", Utils.measureExecTime(QuickSort.quickSortParallel(arrayWith1000000Elem.clone())))
    println("5. Sort array with 10 000 000 elements:")
    printf("   a) Sequential: %d ms", Utils.measureExecTime(QuickSort.quickSort(arrayWith10000000Elem.clone())))
    printf("   b) Parallel: %d ms %n", Utils.measureExecTime(QuickSort.quickSortParallel(arrayWith10000000Elem.clone())))

  }

  def matrixMultiplicationSpeedTest() = {
    val A100 = new Matrix(Array.fill(100)(Array.fill(100)(Random.between(0, 50))))
    val B100 = new Matrix(Array.fill(100)(Array.fill(100)(Random.between(0, 50))))
    val A1000 = new Matrix(Array.fill(1000)(Array.fill(1000)(Random.between(0, 50))))
    val B1000 = new Matrix(Array.fill(1000)(Array.fill(1000)(Random.between(0, 50))))
    val A2000 = new Matrix(Array.fill(2000)(Array.fill(2000)(Random.between(0, 50))))
    val B2000 = new Matrix(Array.fill(2000)(Array.fill(2000)(Random.between(0, 50))))


    println("--------------------------------- Matrix Operation Time Performance ---------------------------------------")
    println("Multiply Matrix 100x100: ")
    printf("   a) Sequential: %d ms", Utils.measureExecTime(A100.multiply(B100)))
    printf("   b) Parallel: %d ms %n", Utils.measureExecTime(A100.multiplyParallel(B100)))
    println("Multiply Matrix 1000x1000: ")
    printf("   a) Sequential: %d ms", Utils.measureExecTime(A1000.multiply(B1000)))
    printf("   b) Parallel: %d ms %n", Utils.measureExecTime(A1000.multiplyParallel(B1000)))
    println("Multiply Matrix 2000x2000: ")
    printf("   a) Sequential: %d ms", Utils.measureExecTime(A2000.multiply(B2000)))
    printf("   b) Parallel: %d ms %n", Utils.measureExecTime(A2000.multiplyParallel(B2000)))
  }
}
