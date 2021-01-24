import org.scalatest.FunSuite

import scala.util.Random

class QuickSortTest extends FunSuite {
  test("sequential") {
    assert {
      val input = Array(1)
      QuickSort.sequential(input)
      Utils.isSorted(input)
    }

    assert {
      val input = Array(3, 1, 2)
      QuickSort.sequential(input)
      Utils.isSorted(input)
    }

    assert {
      val input = Array(-5, 3, -6, 9, -9, 8, 5, -3, -2, 10, -7, 2, -4, -8, 7, -1, 4, 6, -10, 1, 0)
      QuickSort.sequential(input)
      Utils.isSorted(input)
    }

    assert {
      val input = Array.fill(10_000)(Random.nextInt())
      QuickSort.sequential(input)
      Utils.isSorted(input)
    }
  }

  test("concurrent") {
    assert {
      val input = Array(1)
      QuickSort.concurrent(input)
      Utils.isSorted(input)
    }

    assert {
      val input = Array(3, 1, 2)
      QuickSort.concurrent(input)
      Utils.isSorted(input)
    }

    assert {
      val input = Array(-5, 3, -6, 9, -9, 8, 5, -3, -2, 10, -7, 2, -4, -8, 7, -1, 4, 6, -10, 1, 0)
      QuickSort.concurrent(input)
      Utils.isSorted(input)
    }

    assert {
      val input = Array.fill(10_000)(Random.nextInt())
      QuickSort.concurrent(input)
      Utils.isSorted(input)
    }
  }

  test("efficiency") {
    val size = 100_000_000

    val input1 = Array.fill(size)(Random.nextInt())
    val input2 = input1.clone()

    println(s"Sequential: ${Utils.executionTime(() => QuickSort.sequential(input1))} ms")
    assert(Utils.isSorted(input1))

    println(s"Concurrent: ${Utils.executionTime(() => QuickSort.concurrent(input2))} ms")
    assert(Utils.isSorted(input2))

    /*
     * Sequential: 17492 ms
     * Concurrent: 8607 ms
     */
  }
}
