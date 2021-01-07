

import scala.util.Random

import MatrixOperations._

object Tests extends App {

  println("Comparison of sequential and parallel multiplication of matrixes.")

  var xMatrixDimension = 1000
  var yMatrixDimension = 1000
  var commonMatrixDimension = 1000

  println("Matrix A dimensions: "+ xMatrixDimension+" x " + commonMatrixDimension)
  println("Matrix B dimensions: "+ commonMatrixDimension+" x " + yMatrixDimension)
  println("Matrixes A and B are both filled with random integer values.")

  var matA = Array.fill(xMatrixDimension, commonMatrixDimension)(Random.nextInt())
  var matB = Array.fill(commonMatrixDimension, yMatrixDimension)(Random.nextInt())

  /*displayMat(matA)
  println()
  displayMat(matB)
  println()

  println(isEqual(matA,matB))
  println()

  displayMat( multiply(matA, matB))
  println()

  displayMat( multiplyPar(matA, matB))
  println()*/

  var timeSeq:Long = 0
  var timePar:Long = 0

  val time = System.currentTimeMillis()
  multiply(matA, matB)
  timeSeq = System.currentTimeMillis() - time
  println("Time of sequential multiplying: " + timeSeq+ " milliseconds")

  val time2 = System.currentTimeMillis()
  multiplyPar(matA, matB)
  timePar = System.currentTimeMillis() - time2
  println("Time of parallel multiplying: " + timePar+ " milliseconds")

  println("Confirmation of equality of results: "+isEqual(multiply(matA, matB), multiplyPar(matA, matB)))

  println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
}