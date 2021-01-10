import Utils._
import QuickSort._

object Tests {

  def main(args: Array[String]): Unit = {

    // Tests if functions work correctly
    allFunctionsTests()

    // Quicksort int Array
    quickSortTimeTest()

    // Matrices multiply
    multiplyMatricesTimeTest()

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

    assert(Matrix(Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0))).multiplySequential(Matrix(Array(Array(5, 6, 7, 8), Array(9, 3, 4, 2), Array(3, 4, 5, 6)))) == Matrix(Array(Array(32, 24, 30, 30), Array(83, 63, 78, 78))))
    assert(Matrix(Array(Array(10.0, 2.0), Array(4.0, 50.0))).multiplySequential(Matrix(Array(Array(5, 16, 7), Array(9, 30, 4)))) == Matrix(Array(Array(68, 220, 78), Array(470, 1564, 228))))

    assert(Matrix(Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0))).multiplyParallel(Matrix(Array(Array(5, 6, 7, 8), Array(9, 3, 4, 2), Array(3, 4, 5, 6)))) == Matrix(Array(Array(32, 24, 30, 30), Array(83, 63, 78, 78))))
    assert(Matrix(Array(Array(10.0, 2.0), Array(4.0, 50.0))).multiplyParallel(Matrix(Array(Array(5, 16, 7), Array(9, 30, 4)))) == Matrix(Array(Array(68, 220, 78), Array(470, 1564, 228))))
  }

  def quickSortTimeTest(): Unit = {

    println("QUICKSORT TEST\n")

    quickSortTimeTest(1000)
    quickSortTimeTest(10000)
    quickSortTimeTest(100000)
    quickSortTimeTest(1000000)
  }
  /*
  n = 1000
  Quicksort sequential time:	     885332 ns
  Quicksort parallel time:	       376888 ns

  n = 10000
  Quicksort sequential time:	    2129380 ns
  Quicksort parallel time:	      1554171 ns

  n = 100000
  Quicksort sequential time:	   14451440 ns
  Quicksort parallel time:	     10207888 ns

  n = 1000000
  Quicksort sequential time:	  187780914 ns
  Quicksort parallel time:	    159072206 ns
  */

  def quickSortTimeTest(n: Int): Unit = {

    val testArray = generateRandomIntArray(n, -1000, 5000)

    println(s"n = $n")
    calculateTime("Quicksort sequential", quickSortSequential(testArray.clone()))
    calculateTime("Quicksort parallel", quickSortParallel(testArray.clone()))

    println()
  }

  def multiplyMatricesTimeTest(): Unit = {

    println("MULTIPLY MATRICES TEST\n")

    multiplyMatricesTimeTest(2, 3, 3, 4)
    multiplyMatricesTimeTest(20, 30, 30, 40)
    multiplyMatricesTimeTest(200, 300, 300, 400)
    multiplyMatricesTimeTest(2000, 300, 300, 4000)
    multiplyMatricesTimeTest(2000, 3000, 3000, 4000)
    multiplyMatricesTimeTest(1000, 10000, 10000, 1000)
  }
  /*
  A_2x3 x B_3x4
  Multiply sequential  time:	           34370 ns
  Multiply parallel    time:	          894023 ns

  A_20x30 x B_30x40
  Multiply sequential  time:	          678714 ns
  Multiply parallel    time:	        11111089 ns

  A_200x300 x B_300x400
  Multiply sequential  time:	        39170293 ns
  Multiply parallel    time:	       111985161 ns

  A_2000x300 x B_300x4000
  Multiply sequential  time:	      3545971860 ns
  Multiply parallel    time:	      2232137813 ns

  A_2000x3000 x B_3000x4000
  Multiply sequential  time:	     31863359875 ns
  Multiply parallel    time:	     10913083826 ns

  A_1000x10000 x B_10000x1000
  Multiply sequential  time:	     13040980463 ns
  Multiply parallel    time:	      3207014307 ns
  */

  def multiplyMatricesTimeTest(m1: Int, n1: Int, m2: Int, n2: Int): Unit = {

    val testMatrix1 = generateRandomMatrix(m1, n1, -300, 300)
    val testMatrix2 = generateRandomMatrix(m2, n2, -300, 300)

    println(s"A_${m1}x$n1 x B_${m2}x$n2")
    calculateTime("Multiply sequential", testMatrix1.multiplySequential(testMatrix2))
    calculateTime("Multiply parallel", testMatrix1.multiplyParallel(testMatrix2))

    println()
  }

}
