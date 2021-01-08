
import scala.collection.parallel.CollectionConverters._

// implemented matrix operations:
// display
// isEqual (both parallel and sequential)
// multiply (both parallel and sequential)

object MatrixOperations {

  def displayMat(mat: Array[Array[Int]]): Unit = {
    for (i <- mat.indices) {
      for (j <- mat(0).indices) {
        print(" " + mat(i)(j))
      }
      println()
    }
  }

  def isEqual[A](matrixA: Array[Array[Int]], matrixB: Array[Array[Int]]): Boolean = {
    assert(matrixA.length > 0 && matrixB.length > 0 && matrixA.length == matrixB.length && matrixA(0).length == matrixB(0).length)
    for (i <- matrixA.indices)
      for (j <- matrixA.indices)
        if (matrixA(i)(j) != matrixB(i)(j))
          return false
    true
  }

  def isEqualParallel[A](matrixA: Array[Array[Int]], matrixB: Array[Array[Int]]): Boolean = {
    assert(matrixA.length > 0 && matrixB.length > 0 && matrixA.length == matrixB.length && matrixA(0).length == matrixB(0).length)
    for (k <- (0 to 1).par) {
      for (i <- k * matrixA.length / 2 until (matrixA.length - matrixA.length / 2) * k + matrixA.length / 2) {
        for (j <- matrixA(0).indices) {
          if (matrixA(i)(j) != matrixB(i)(j)) {
            false
          }
        }
      }
    }
    true
  }

  def multiply[A](matrixA: Array[Array[Int]], matrixB: Array[Array[Int]]): Array[Array[Int]] = {
    assert(matrixA.length > 0 && matrixB.length > 0 &&
      matrixA(0).length > 0 && matrixB(0).length > 0 &&
      matrixA.length == matrixB.length &&
      matrixA(0).length == matrixB.length)

    var retMatrix = Array.fill(matrixA.length,matrixB(0).length)(0)

    for (i <- retMatrix.indices) {
      for (j <- retMatrix(0).indices) {
        for (r <- matrixB(0).indices) {
          retMatrix(i)(j) += matrixA(i)(r) * matrixB(r)(j)
        }
      }
    }
    retMatrix
  }

  def multiplyPar[A](matrixA: Array[Array[Int]], matrixB: Array[Array[Int]]): Array[Array[Int]] = {
    assert(matrixA.length > 0 && matrixB.length > 0 &&
      matrixA(0).length > 0 && matrixB(0).length > 0 &&
      matrixA.length == matrixB.length &&
      matrixA(0).length == matrixB.length)

    var retMatrix = Array.fill(matrixA.length,matrixB(0).length)(0)

    for (i <- retMatrix.indices.par) {
      for (j <- retMatrix(0).indices) {
        for (r <- matrixB(0).indices) {
          retMatrix(i)(j) += matrixA(i)(r) * matrixB(r)(j)
        }
      }
    }
    retMatrix
  }

}
