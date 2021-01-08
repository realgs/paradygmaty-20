

import scala.util.Random

import MatrixOperations._

object Tests extends App {

  var arrA = Array.fill(9, 9)(Random.nextInt())
  var arrB = Array.fill(9, 9)(Random.nextInt())

  for (i <- 0 until 9) {
    for (j <- 0 until 9) {
      print(" " + arrA(i)(j));
    }
    println();
  }

  for (i <- 0 until 9) {
    for (j <- 0 until 9) {
      print(" " + arrB(i)(j));
    }
    println();
  }

  println(isEqual(arrA, arrA));
  println(isEqual(arrA, arrB));


}



import scala.util.Random

import MatrixOperations._
import CodeBreaking._

// I use .par method from scala.collection.parallel.CollectionConverters in order to parallel compute.

// I have implemented multiplying of matrixes (stored as Array[Array[Int]]) and breaking integer N-digit code with
// brute force algorithm, both sequential and parallel.

// Exemplary result for multiplying matrixes 1000x1000:
//    Time of sequential multiplying: 1932 milliseconds
//    Time of parallel multiplying: 763 milliseconds

// Exemplary result for finding 9 digit code:
//    Average time of sequential code breaking: 2325 milliseconds
//    Average time of parallel code breaking: 777 milliseconds

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
  println()

  val codeLength = 9
  val numberOfRepeats = 10
  println("Breaking "+codeLength+" digit code.")
  println("Due to high randomness, final result will be an average from " + numberOfRepeats + " attempts of breaking different codes.")

  var timeStart:Long = 0
  var timeEnd:Long = 0
  var totalSeqTime:Long =0
  var totalParTime:Long =0
  var confirmationCheck:Int = 0

  var i = 0
  while(i < numberOfRepeats) {
    val codeToBreak = new CodeBreaking(codeLength)

    timeStart = System.currentTimeMillis()
    confirmationCheck = bruteForce(codeToBreak)
    timeEnd = System.currentTimeMillis()

    assert(confirmSecretCode(codeToBreak,confirmationCheck))
    totalSeqTime += timeEnd - timeStart

    timeStart = System.currentTimeMillis()
    confirmationCheck = bruteForcePar(codeToBreak)
    timeEnd = System.currentTimeMillis()

    assert(confirmSecretCode(codeToBreak,confirmationCheck))
    totalParTime+=timeEnd - timeStart

    i += 1
  }
  println("Average time of sequential code breaking: " + totalSeqTime/numberOfRepeats+ " milliseconds")
  println("Average time of parallel code breaking: " + totalParTime/numberOfRepeats+ " milliseconds")

}

