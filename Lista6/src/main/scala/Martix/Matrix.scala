package Martix
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import ParallelMachine.ParallelMachine._

//in my case, matrix is immutable. Multiplication method will return new matrix. It won't change state of current matrix (no OOP)
class Matrix (array: Array[Array[Int]]) {
  require(array.length > 0, s"Matrix can't be empty: rows length is 0 or negative")
  require(array(0).length > 0, s"Matrix can't be empty: columns length is 0 or negative")

  private val rowsAmount: Int = array.length
  private val columnsAmount: Int = array(0).length // I assume that every row must have the same length as the first row

  for(i <- array.indices) {
    require(array(i).length == columnsAmount, s"Columns in matrix can't have different size: required $columnsAmount")
  }

  private val innerMatrix: Array[Array[Int]] =  array
  private val transMatrix: Array[Array[Int]] = innerMatrix.transpose // will be useful for multiplication (for SIMD operation)

  // getters (no setters bc I want it to be immutable matrix)
  def matrix: Array[Array[Int]] = innerMatrix
  def rowsLength: Int = rowsAmount
  def columnsLength: Int = columnsAmount
  def transposeMatrix: Array[Array[Int]] = transMatrix

  //for tests
  def printMatrix(): Unit = {
    for(i <- 0 until rowsAmount) {
      for(j <- 0 until columnsAmount)
        print(innerMatrix(i)(j)+" ")
      println(" ")
    }
    println(" ")
  }

  //for equality tests
  def ifEqualMatrices(otherMatrix: Matrix): Boolean = {
    if(rowsAmount != otherMatrix.rowsAmount)
      false
    else if(columnsAmount != otherMatrix.columnsAmount)
      false
    else {
      var ifEqual = true
      for(i <- 0 until rowsAmount) {
        for(j <- 0 until columnsAmount) {
          if(innerMatrix(i)(j) != otherMatrix.matrix(i)(j)) {
            ifEqual = false
          }
        }
      }
      ifEqual
    }
  }

  //multiplication
  private def multiplyNormal(otherMatrix: Matrix, startRowThisMatrix: Int, endRowThisMatrix: Int, newMatrix: Array[Array[Int]]): Unit = {
    val otherTransMatrix = otherMatrix.transMatrix
    for(thisMatrixRow <- startRowThisMatrix to endRowThisMatrix) {
      for(otherMatrixRow <- otherTransMatrix.indices) {
        newMatrix(thisMatrixRow)(otherMatrixRow) = (innerMatrix(thisMatrixRow) zip otherTransMatrix(otherMatrixRow) map Function.tupled(_ * _)).sum
      }
    }
  }


  def multiply(otherMatrix: Matrix): Matrix = {
    require(columnsAmount == otherMatrix.rowsAmount, s"Can't multiply these matrices. Incompatible matrices dimensions")
    val newMatrix = Array.ofDim[Int](rowsAmount, otherMatrix.columnsAmount)
    multiplyNormal(otherMatrix, 0, rowsAmount - 1, newMatrix)
    new Matrix(newMatrix)
  }


  def multiplyFuture(otherMatrix: Matrix): Matrix = {
    require(columnsAmount == otherMatrix.rowsAmount, s"Can't multiply these matrices. Incompatible matrices dimensions")
    val newMatrix = Array.ofDim[Int](rowsAmount, otherMatrix.columnsAmount)
    val midRowIndex = rowsAmount / 2
    val fut1 = Future(multiplyNormal(otherMatrix, 0, midRowIndex - 1, newMatrix))
    val fut2 = Future(multiplyNormal(otherMatrix, midRowIndex, rowsAmount - 1, newMatrix))
    Await.result(fut1, 1000.seconds)                                  // waits at most 1000 seconds for the result
    Await.result(fut2, 1000.seconds)                                  // safer than Duration.Inf
    new Matrix(newMatrix)
  }

  def multiplyParallel(otherMatrix: Matrix): Matrix = {
    require(columnsAmount == otherMatrix.rowsAmount, s"Can't multiply these matrices. Incompatible matrices dimensions")
    val newMatrix = Array.ofDim[Int](rowsAmount, otherMatrix.columnsAmount)
    val midRowIndex = rowsAmount / 2
    parallel(multiplyNormal(otherMatrix, 0, midRowIndex - 1, newMatrix), multiplyNormal(otherMatrix, midRowIndex, rowsAmount - 1, newMatrix))
    new Matrix(newMatrix)
  }
}
