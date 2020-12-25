import org.scalatest.FunSuite
import Martix.Matrix

class MatrixMultiplicationTest extends FunSuite {

  test("MatrixMultiplication - multiply correctness test 1") {
    val m1: Matrix = new Matrix(Array(Array(1,2), Array(3,4)))
    val m2: Matrix = new Matrix(Array(Array(-3,-8,3), Array(-2, 1, 4)))
    val m3 = m1.multiply(m2)
    val correctResult = new Matrix(Array(Array(-7,-6,11), Array(-17,-20,25)))
    assert(correctResult.ifEqualMatrices(m3))
  }

  test("MatrixMultiplication - multiply correctness test 2") {
    val m1: Matrix = new Matrix(Array(
      Array(5,11,0,2),
      Array(13,9,4,6),
      Array(4,-1,4,-2),
      Array(1,-2,3,0)
    ))

    val m2: Matrix = new Matrix(Array(
      Array(1,0,9,5),
      Array(-6,-1,2,1),
      Array(-2,-5,3,3),
      Array(1,5,9,0)
    ))

    val m3 = m1.multiply(m2)

    val correctResult = new Matrix(Array(
      Array(-59,-1,85,36),
      Array(-43,1,201,86),
      Array(0,-29,28,31),
      Array(7,-13,14,12)
    ))

    assert(correctResult.ifEqualMatrices(m3))
  }


  test("MatrixMultiplication - multiplyFuture correctness test 1") {
    val m1: Matrix = new Matrix(Array(Array(1,2), Array(3,4)))
    val m2: Matrix = new Matrix(Array(Array(-3,-8,3), Array(-2, 1, 4)))
    val m3 = m1.multiplyFuture(m2)
    val correctResult = new Matrix(Array(Array(-7,-6,11), Array(-17,-20,25)))
    assert(correctResult.ifEqualMatrices(m3))
  }

  test("MatrixMultiplication - multiplyFuture correctness test 2") {
    val m1: Matrix = new Matrix(Array(
      Array(5,11,0,2),
      Array(13,9,4,6),
      Array(4,-1,4,-2),
      Array(1,-2,3,0)
    ))

    val m2: Matrix = new Matrix(Array(
      Array(1,0,9,5),
      Array(-6,-1,2,1),
      Array(-2,-5,3,3),
      Array(1,5,9,0)
    ))

    val m3 = m1.multiplyFuture(m2)

    val correctResult = new Matrix(Array(
      Array(-59,-1,85,36),
      Array(-43,1,201,86),
      Array(0,-29,28,31),
      Array(7,-13,14,12)
    ))

    assert(correctResult.ifEqualMatrices(m3))
  }


  test("MatrixMultiplication - multiplyParallel correctness test 1") {
    val m1: Matrix = new Matrix(Array(Array(1,2), Array(3,4)))
    val m2: Matrix = new Matrix(Array(Array(-3,-8,3), Array(-2, 1, 4)))
    val m3 = m1.multiplyParallel(m2)
    val correctResult = new Matrix(Array(Array(-7,-6,11), Array(-17,-20,25)))
    assert(correctResult.ifEqualMatrices(m3))
  }

  test("MatrixMultiplication - multiplyParallel correctness test 2") {
    val m1: Matrix = new Matrix(Array(
      Array(5,11,0,2),
      Array(13,9,4,6),
      Array(4,-1,4,-2),
      Array(1,-2,3,0)
    ))

    val m2: Matrix = new Matrix(Array(
      Array(1,0,9,5),
      Array(-6,-1,2,1),
      Array(-2,-5,3,3),
      Array(1,5,9,0)
    ))

    val m3 = m1.multiplyParallel(m2)

    val correctResult = new Matrix(Array(
      Array(-59,-1,85,36),
      Array(-43,1,201,86),
      Array(0,-29,28,31),
      Array(7,-13,14,12)
    ))

    assert(correctResult.ifEqualMatrices(m3))
  }

}
