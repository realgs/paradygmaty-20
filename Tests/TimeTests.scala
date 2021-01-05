package Tests

import Algorithms.Matrix

object TimeTests {
  def main(args: Array[String]): Unit = {
    val A: Matrix = new Matrix(Array(Array(5, 4, -10, 1), Array(-1, 9, 0, 2), Array(0, 4, 7, 3)))
    val B: Matrix = new Matrix(Array(Array(1,6,2), Array(-5, -2, 13), Array(5,1,-3)))

    A.printMatrix()
    B.printMatrix()

  }
}
