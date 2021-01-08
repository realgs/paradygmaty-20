

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
