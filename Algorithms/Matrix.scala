package Algorithms

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
    require(this.columnsLength == secondMatrix.rowsLength, s"Incorrect properties of matrixes.")

    val resultArray: Array[Array[Int]] = Array.ofDim(this.rowsLength, secondMatrix.columnsLength)
    for (i <- 0 until this.rowsLength)
      for (j <- 0 until this.columnsLength)
        for (k <- 0 until secondMatrix.columnsLength) {
          resultArray(i)(j) = resultArray(i)(j) + this.get(i)(k) * secondMatrix.get(k)(j)
        }
    new Matrix(resultArray)
  }

  def printMatrix() = {
    for (i <- 0 until this.rowsLength) {
      for (j <- 0 until this.columnsLength)
        print(matrix(i)(j) + " ")
      println(" ")
    }
    println(" ")
  }
}
