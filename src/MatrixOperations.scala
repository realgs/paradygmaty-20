
import scala.collection.parallel.CollectionConverters._

object MatrixOperations {

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
      for (i <- (k * matrixA.length / 2 until (matrixA.length - matrixA.length / 2) * k + matrixA.length / 2)) {
        for (j <- 0 until matrixA(0).length) {
          if (matrixA(i)(j) != matrixB(i)(j)) {
            false
          }
        }
      }
    }
    true
  }
}