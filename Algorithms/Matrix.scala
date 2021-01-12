package Algorithms

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class Matrix(private[this] val rows: Int, private[this] val columns: Int, private[this] val array: Array[Array[Int]]) {
  def columnsLength: Int = columns

  def rowsLength: Int = rows

  def matrix: Array[Array[Int]] = array

  def this(rows: Int, columns: Int) = {
    this(columns, rows, Array.ofDim(columns, rows))
  }

  def this(array: Array[Array[Int]]) = {
    this(array.length, array(0).length, array)
  }

  def get(row: Int)(column: Int): Int = matrix(row)(column)

  def multiply(secondMatrix: Matrix): Matrix = {
    validate(this, secondMatrix)
    val resultArray: Array[Array[Int]] = Array.ofDim(this.rowsLength, secondMatrix.columnsLength)
    for (i <- 0 until this.rowsLength)
      for (j <- 0 until this.columnsLength)
        for (k <- 0 until secondMatrix.columnsLength) {
          resultArray(i)(j) = resultArray(i)(j) + this.get(i)(k) * secondMatrix.get(k)(j)
        }
    new Matrix(resultArray)
  }

  def multiplyParallel(secondMatrix: Matrix): Matrix = {
    validate(this, secondMatrix)

    val resultArray: Array[Array[Int]] = Array.ofDim(this.rowsLength, secondMatrix.columnsLength)
    val availableThreads = 4 //using only 4 threads
    val rowsPerThread = Math.ceil(this.rowsLength / availableThreads).toInt
    var listOfThreads: List[Future[Unit]] = List()

    for (i <- 0 until availableThreads) {
      val border = (i * rowsPerThread, Math.min(this.rowsLength, (i + 1) * rowsPerThread))
      listOfThreads = Future(multiplyRows(border, secondMatrix, resultArray)) :: listOfThreads
    }

    for (i <- listOfThreads.indices) {
      Await.ready(listOfThreads(i), 1000.seconds)
    }

    new Matrix(resultArray)
  }

  def multiplyRows(border: (Int, Int), secondMatrix: Matrix, result: Array[Array[Int]]): Unit = {
    for (i <- border._1 until border._2)
      for (j <- 0 until secondMatrix.columnsLength)
        for (k <- 0 until secondMatrix.columnsLength) {
          result(i)(j) = result(i)(j) + this.get(i)(k) * secondMatrix.get(k)(j)
        }
  }

  def validate(firstMatrix: Matrix, secondMatrix: Matrix): Unit = {
    require(firstMatrix.columnsLength == secondMatrix.rowsLength, s"Incorrect properties of matrixes.")
  }

  def printMatrix(): Unit = {
    for (i <- 0 until this.rowsLength) {
      for (j <- 0 until this.columnsLength)
        print(matrix(i)(j) + " ")
      println(" ")
    }
    println(" ")
  }

  def isEqual(secondMatrix: Matrix): Boolean = {
    if (this.rowsLength != secondMatrix.rowsLength)
      return false
    if (this.columnsLength != secondMatrix.columnsLength)
      return false

    var isEqual = true
    for (i <- 0 until this.rowsLength) {
      for (j <- 0 until this.columnsLength) {
        if (this.get(i)(j) != secondMatrix.get(i)(j))
          isEqual = false
      }
    }

    isEqual
  }
}
