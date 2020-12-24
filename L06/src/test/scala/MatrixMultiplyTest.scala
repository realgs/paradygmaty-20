import org.scalatest.FunSuite

import scala.util.Random

class MatrixMultiplyTest extends FunSuite {
  test("concurrent") {
    assert {
      val a = Array(
        Array(1, 2, 2),
        Array(1, 3, 3),
        Array(2, 2, 1)
      )
      val b = Array(
        Array(1, 1, 2),
        Array(2, 1, 3),
        Array(2, 1, 2)
      )

      val result: Array[Array[Int]] = MatrixMultiply.concurrent(a, b)

      val expected = Array(
        Array(9, 5, 12),
        Array(13, 7, 17),
        Array(8, 5, 12)
      )

      result.zip(expected).forall { case (row1, row2) => row1.sameElements(row2) }
    }

    assert {
      val a = Array(
        Array(1, 2, 2),
        Array(1, 3, 3),
        Array(2, 2, 1)
      )
      val b = Array(
        Array(2),
        Array(2),
        Array(2)
      )

      val result: Array[Array[Int]] = MatrixMultiply.concurrent(a, b)

      val expected = Array(
        Array(10),
        Array(14),
        Array(10)
      )

      result.zip(expected).forall { case (row1, row2) => row1.sameElements(row2) }
    }
  }

  test("sequential") {
    assert {
      val a = Array(
        Array(1, 2, 2),
        Array(1, 3, 3),
        Array(2, 2, 1)
      )
      val b = Array(
        Array(1, 1, 2),
        Array(2, 1, 3),
        Array(2, 1, 2)
      )

      val result: Array[Array[Int]] = MatrixMultiply.sequential(a, b)

      val expected = Array(
        Array(9, 5, 12),
        Array(13, 7, 17),
        Array(8, 5, 12)
      )

      result.zip(expected).forall { case (row1, row2) => row1.sameElements(row2) }
    }

    assert {
      val a = Array(
        Array(1, 2, 2),
        Array(1, 3, 3),
        Array(2, 2, 1)
      )
      val b = Array(
        Array(2),
        Array(2),
        Array(2)
      )

      val result: Array[Array[Int]] = MatrixMultiply.sequential(a, b)

      val expected = Array(
        Array(10),
        Array(14),
        Array(10)
      )

      result.zip(expected).forall { case (row1, row2) => row1.sameElements(row2) }
    }
  }

  test("efficiency") {
    val r = Random
    val rows = 1500
    val columns = 1500

    val a = Array.fill(rows)(Array.fill(columns)(r.nextInt(100)))
    val b = Array.fill(rows)(Array.fill(columns)(r.nextInt(100)))

    println(s"Sequential: ${Utils.executionTime(() => MatrixMultiply.sequential(a, b))} ms")
    println(s"Concurrent: ${Utils.executionTime(() => MatrixMultiply.concurrent(a, b))} ms")

    /*
     * Sequential: 32418 ms
     * Concurrent: 9011 ms
     */
  }
}
