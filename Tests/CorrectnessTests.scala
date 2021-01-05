package Tests

import Algorithms.{Fibonacci, Matrix, QuickSort}
import org.junit.jupiter.api.{Assertions, Test}

import scala.util.Random

class CorrectnessTests {
  @Test
  def FibonacciTest(): Unit = {
    //testing sequential algorithm
    assert(Fibonacci.findFibonacci(25) == 75025)
    assert(Fibonacci.findFibonacci(30) == 832040)
    assert(Fibonacci.findFibonacci(0) == 0)
    assert(Fibonacci.findFibonacci(1) == 1)

    //testing sequential algorithm with tail recursion
    assert(Fibonacci.findFibonacciIter(25) == 75025)
    assert(Fibonacci.findFibonacciIter(30) == 832040)
    assert(Fibonacci.findFibonacciIter(0) == 0)
    assert(Fibonacci.findFibonacciIter(1) == 1)

    //testing parallel algorithm
    assert(Fibonacci.findFibonacciParallel(25) == 75025)
    assert(Fibonacci.findFibonacciParallel(30) == 832040)
    assert(Fibonacci.findFibonacciParallel(0) == 0)
    assert(Fibonacci.findFibonacciParallel(1) == 1)
  }

  @Test
  def QuickSortTest(): Unit = {
    /*testing sequential algorithm*/

    //array with 3 elements
    var testArray1 = Array(3, 1, 2)
    QuickSort.quickSort(testArray1)
    Assertions.assertArrayEquals(Array(1, 2, 3), testArray1)

    //array with 1 element
    testArray1 = Array(1)
    QuickSort.quickSort(testArray1)
    Assertions.assertArrayEquals(Array(1), testArray1)

    //array with 100 elements randomly generated
    testArray1 = Array.fill(100)(Random.nextInt())
    val testArray2 = testArray1.clone()
    QuickSort.quickSort(testArray1)
    scala.util.Sorting.quickSort(testArray2)
    Assertions.assertArrayEquals(testArray2, testArray1)

    /*testing parallel algorithm*/

    //array with 3 elements
    var testArrayParallel1 = Array(3, 1, 2)
    QuickSort.quickSort(testArrayParallel1)
    Assertions.assertArrayEquals(Array(1, 2, 3), testArrayParallel1)

    //array with 1 element
    testArrayParallel1 = Array(1)
    QuickSort.quickSort(testArrayParallel1)
    Assertions.assertArrayEquals(Array(1), testArrayParallel1)

    //array with 100 elements randomly generated
    testArrayParallel1 = Array.fill(100)(Random.nextInt())
    val testArrayParallel2 = testArrayParallel1.clone()
    QuickSort.quickSort(testArrayParallel1)
    scala.util.Sorting.quickSort(testArrayParallel2)
    Assertions.assertArrayEquals(testArrayParallel2, testArrayParallel1)

  }

  @Test
  def matrixMultiplicationTest(): Unit = {
    var A: Matrix = new Matrix(Array(
      Array(5, -1, 0),
      Array(4, 9, 4),
      Array(-10, 0, 7),
      Array(1, 2, 3)
    ))
    var B: Matrix = new Matrix(Array(
      Array(1, -5, 5),
      Array(6, -2, 1),
      Array(2, 13, -3)
    ))
    //correct product
    val C: Matrix = new Matrix(Array(
      Array(-1, -23, 24),
      Array(66, 14, 17),
      Array(4, 141, -71),
      Array(19, 30, -2)
    ))

    assert(A.multiply(B).isEqual(C))
    assert(A.multiplyParallel(B).isEqual(C))

    //randomly generated 100x100 Matrix
    A = new Matrix(Array.fill(100)(Array.fill(100)(Random.nextInt(500))))
    B = new Matrix(Array.fill(100)(Array.fill(100)(Random.nextInt(500))))

    assert(A.multiply(B).isEqual(A.multiplyParallel(B)))
  }
}
