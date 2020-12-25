import org.scalatest.FunSuite

import scala.util.Random

class QuickSortTest extends FunSuite {
  test("sequential") {
    assert {
      val input = Array(1)
      val expected = Array(1)
      QuickSort.sequential(input)
      input.sameElements(expected)
    }

    assert {
      val input = Array(3, 1, 2)
      val expected = Array(1, 2, 3)
      QuickSort.sequential(input)
      input.sameElements(expected)
    }

    assert {
      val input = Array(-5, 3, -6, 9, -9, 8, 5, -3, -2, 10, -7, 2, -4, -8, 7, -1, 4, 6, -10, 1, 0)
      val expected = Array(-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      QuickSort.sequential(input)
      input.sameElements(expected)
    }
  }

  test("concurrent") {
    assert {
      val input = Array(1)
      val expected = Array(1)
      QuickSort.concurrent(input)
      input.sameElements(expected)
    }

    assert {
      val input = Array(3, 1, 2)
      val expected = Array(1, 2, 3)
      QuickSort.concurrent(input)
      input.sameElements(expected)
    }

    assert {
      val input = Array(-5, 3, -6, 9, -9, 8, 5, -3, -2, 10, -7, 2, -4, -8, 7, -1, 4, 6, -10, 1, 0)
      val expected = Array(-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      QuickSort.concurrent(input)
      input.sameElements(expected)
    }
  }

  test("efficiency") {
    val size = 100_000_000
    println(s"Sequential: ${Utils.executionTime(() => QuickSort.sequential(Array.fill(size)(Random.nextInt())))} ms")
    println(s"Concurrent: ${Utils.executionTime(() => QuickSort.concurrent(Array.fill(size)(Random.nextInt())))} ms")

    /*
     * Sequential: 14635 ms
     * Concurrent: 1765 ms
     */
  }
}
