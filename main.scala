import util._

object Main{

  //------------problem 1: matrix multiplication-------------//
  def scalarArrayMul(arr1: Array[Int], arr2: Array[Int]): Int ={
    if(arr1.length != arr2.length) throw new Exception("Invalid arrays length: scalarArrayMul()")
    var result = 0
    for(i <- 0 to arr1.length - 1){
      result += arr1(i)*arr2(i)
    }
    result
  }
  // default matrix multiplication
  def matrixMul(mtx1: Matrix, mtx2: Matrix) ={
    val resMatrix = new Matrix(mtx1.rowsNum, mtx2.colNum, false)
    for(i <- 0 to resMatrix.rowsNum - 1){
      for(j <- 0 to resMatrix.colNum - 1){
        val scalarMulRes = scalarArrayMul(mtx1.getRow(i), mtx2.getCol(j))
        resMatrix.setAtIndex(i, j, scalarMulRes)
      }
    }
    resMatrix
  }

  // parallel matrix multiplication
  def parMatrixMul(mtx1: Matrix, mtx2: Matrix) ={
    val divVal = mtx1.rowsNum/2 // dividing matrix1 into 2
    val mtx1_1_array = new Array[Int](mtx1.colNum * divVal)
    for (i <- 0 to mtx1_1_array.length - 1){
      mtx1_1_array(i) = mtx1.matrixArray(i)  // dividing old matrixArray into to halves
    }
    val mtx1_1 = new Matrix(mtx1_1_array, divVal, mtx1.colNum)  // creating first half matrix
    val mtx1_2_array = new Array[Int](mtx1.colNum * (mtx1.rowsNum - divVal))
    var j = mtx1.colNum * divVal
    for (i <- 0 to mtx1_2_array.length - 1){
      mtx1_2_array(i) = mtx1.matrixArray(j) // second half matrixArray
      j += 1
    }
    val mtx1_2 = new Matrix(mtx1_2_array, mtx1.rowsNum - divVal , mtx1.colNum)  // creating second half matrix

    val (resMatrix1, resMatrix2) = parallel(matrixMul(mtx1_1, mtx2), matrixMul(mtx1_2, mtx2)) // two small matrices are multiplied with second matrix in parallel
    new Matrix(resMatrix1, resMatrix2)  // result matrix is composed of upper and lower halves
  }

  def matrixMulTest(): Unit ={
    var m1 = new Matrix(Array(1,2,3,4,5,4,3,2,1), 3, 3)
    var m2 = new Matrix(Array(0,0,0,1,1,1,0,0,0), 3, 3)
    m1.printMatrix
    m2.printMatrix
    matrixMul(m1,m2).printMatrix
    println("Default: " + (matrixMul(m1,m2).matrixArray.toList == List(2,2,2,5,5,5,2,2,2)))
    println("Parallel: " + (parMatrixMul(m1,m2).matrixArray.toList == List(2,2,2,5,5,5,2,2,2)))

    m1 = new Matrix(Array(0,0,0,0), 2, 2)
    m2 = new Matrix(Array(60,20,10,40), 2, 2)
    m1.printMatrix
    m2.printMatrix
    matrixMul(m1,m2).printMatrix
    println("Default: " + (matrixMul(m1,m2).matrixArray.toList == List(0,0,0,0)))
    println("Parallel: " + (parMatrixMul(m1,m2).matrixArray.toList == List(0,0,0,0)))

    m1 = new Matrix(Array(1,1,-1,2,0,1,1,1,2), 3, 3)
    m2 = new Matrix(Array(1,1,0,0,1,1,0,1,1), 3, 3)
    m1.printMatrix
    m2.printMatrix
    matrixMul(m1,m2).printMatrix
    println("Default: " + (matrixMul(m1,m2).matrixArray.toList == List(1,1,0,2,3,1,1,4,3)))
    println("Parallel: " + (parMatrixMul(m1,m2).matrixArray.toList == List(1,1,0,2,3,1,1,4,3)))
  }

  def matrixMulTimeTests(): Unit ={
    println("Multiplication two 10 X 10 matrices:")
    var m1 = new Matrix(10,  true)
    var m2 = new Matrix(10,  true)
    print("default: ")
    time(matrixMul(m1, m2))
    print("parallel: ")
    time(parMatrixMul(m1, m2))

    println
    println("Multiplication two 50 X 50 matrices:")
    m1 = new Matrix(50,  true)
    m2 = new Matrix(50,  true)
    print("default: ")
    time(matrixMul(m1, m2))
    print("parallel: ")
    time(parMatrixMul(m1, m2))

    println
    println("Multiplication two 100 X 100 matrices:")
    m1 = new Matrix(100,  true)
    m2 = new Matrix(100,  true)
    print("default: ")
    time(matrixMul(m1, m2))
    print("parallel: ")
    time(parMatrixMul(m1, m2))

    println
    println("Multiplication two 500 X 500 matrices:")
    m1 = new Matrix(500,  true)
    m2 = new Matrix(500,  true)
    print("default: ")
    time(matrixMul(m1, m2))
    print("parallel: ")
    time(parMatrixMul(m1, m2))

    println
    println("Multiplication two 700 X 700 matrices:")
    m1 = new Matrix(700,  true)
    m2 = new Matrix(700,  true)
    print("default: ")
    time(matrixMul(m1, m2))
    print("parallel: ")
    time(parMatrixMul(m1, m2))

    println
    println("Multiplication two 1000 X 1000 matrices:")
    m1 = new Matrix(1000,  true)
    m2 = new Matrix(1000,  true)
    print("default: ")
    time(matrixMul(m1, m2))
    print("parallel: ")
    time(parMatrixMul(m1, m2))
  }

  //------------problem 2: merge sort------------------------//
  // default mergesort
  def mergesort[T](pred: (T, T) => Boolean, xs: List[T]): List[T] = {
    val lists = listDivide(xs)  // list division into two halves
    lists match{
      case (Nil, a) => a
      case (a, Nil) => a
      case (h1 :: t1, h2 :: t2) => merge(pred, mergesort(pred, h1 :: t1), mergesort(pred, h2 :: t2))
    }
  }

  // parallel mergesort
  def parMergesort[T](pred: (T, T) => Boolean, xs: List[T]): List[T] ={
    val lists = listDivide(xs)
    val (l1, l2) = parallel(mergesort(pred, lists._1), mergesort(pred, lists._2))
    merge(pred, l1, l2)
  }

  def mergeSortTest(): Unit= {
    def pred (a: Int, b: Int): Boolean = a < b
    println(mergesort(pred, List()) == List())
    println(parMergesort(pred, List()) == List())
    println(mergesort(pred, List(6,7,5,2,3)) == List(2,3,5,6,7))
    println(parMergesort(pred, List(6,7,5,2,3)) == List(2,3,5,6,7))
    println(mergesort(pred, List(999,0,1)) == List(0,1,999))
    println(parMergesort(pred, List(999,0,1)) == List(0,1,999))
  }

  def mergeSortTimeTest(): Unit ={
    def pred (a: Int, b: Int): Boolean = a < b
    println("Mergesorting two size 10 lists:")
    print("default: ")
    var list = randomListGen(10)
    time(mergesort(pred, list))
    print("parallel: ")
    time(parMergesort(pred, list))

    println
    println("Mergesorting two size 100 lists:")
    print("default: ")
    list = randomListGen(100)
    time(mergesort(pred, list))
    print("parallel: ")
    time(parMergesort(pred, list))

    println
    println("Mergesorting two size 1000 lists:")
    print("default: ")
    list = randomListGen(1000)
    time(mergesort(pred, list))
    print("parallel: ")
    time(parMergesort(pred, list))

    println
    println("Mergesorting two size 10000 lists:")
    print("default: ")
    list = randomListGen(10000)
    time(mergesort(pred, list))
    print("parallel: ")
    time(parMergesort(pred, list))

    println
    println("Mergesorting two size 30000 lists:")
    print("default: ")
    list = randomListGen(30000)
    time(mergesort(pred, list))
    print("parallel: ")
    time(parMergesort(pred, list))

    println
    println("Mergesorting two size 50000 lists:")
    print("default: ")
    list = randomListGen(50000)
    time(mergesort(pred, list))
    print("parallel: ")
    time(parMergesort(pred, list))
  }

  //------------problem 3: calculating determinant------------//
  // default determinant calc
  def determinantCalc(matrix: Matrix): Double={
    val laplaceRow = matrix.getRow(0) // row toward which Laplace expansion will be calculated
    var result: Double = 0

    if(matrix.colNum == 1){
      result = matrix.matrixArray(0)
    }
    else {
      for (i <- 0 to laplaceRow.length - 1) { // each elem from laplaceRow will have its minor matrix
        val laplaceElemVal = laplaceRow(i)
        val sign = scala.math.pow(-1, i) // sign in final product
        val minor = new Matrix(matrix, 0, i)
        result += sign * determinantCalc(minor) * laplaceElemVal
      }
    }
    result
  }

  //parallel determinant calc
  def parDeterminantCalc(matrix: Matrix): Double={
    def helper(matrix: Matrix, lowerRange: Boolean = false, upperRange: Boolean = false): Double={
      val laplaceRow = matrix.getRow(0) // row toward which Laplace expansion will be calculated
      var result: Double = 0

      val divN: Int = laplaceRow.length / 2
      var range = 0 to laplaceRow.length - 1
      if(lowerRange){
        range = 0 to (divN - 1);
      }
      else if(upperRange){
        range = divN to (laplaceRow.length - 1);
      }

      if(matrix.colNum == 1){
        result = matrix.matrixArray(0)
      }
      else {
        for (i <- range) { // each elem from laplaceRow will have its minor matrix
          val laplaceElemVal = laplaceRow(i)
          val sign = scala.math.pow(-1, i) // sign in final product
          val minor = new Matrix(matrix, 0, i)
          result += sign * helper(minor) * laplaceElemVal
        }
      }
      result
    }
    val (res1, res2) = parallel(helper(matrix, true), helper(matrix, false, true))
    res1 + res2
  }

  def determinantCalcTest(): Unit ={
    var m1 = new Matrix(Array(1,2,3,4),2,2)
    println(determinantCalc(m1) == - 2)
    println(parDeterminantCalc(m1) == -2)
    m1 = new Matrix(Array(0,0,90,90),2,2)
    println(determinantCalc(m1) == 0)
    println(parDeterminantCalc(m1) == 0)
    m1 = new Matrix(Array(0,1,0,4,5,6,2,2,2),3,3)
    println(determinantCalc(m1) == 4)
    println(parDeterminantCalc(m1) == 4)
  }

  def determinantTimeTest(): Unit ={
    println("Determinant calculation for matrix size 2 x 2:")
    print("default: ")
    var m = new Matrix(2, 2, true)
    time(determinantCalc(m))
    print("parallel: ")
    time(parDeterminantCalc(m))

    println
    println("Determinant calculation for matrix size 3 x 3:")
    print("default: ")
    m = new Matrix(3, 3, true)
    time(determinantCalc(m))
    print("parallel: ")
    time(parDeterminantCalc(m))

    println
    println("Determinant calculation for matrix size 4 x 4:")
    print("default: ")
    m = new Matrix(4, 4, true)
    time(determinantCalc(m))
    print("parallel: ")
    time(parDeterminantCalc(m))

    println
    println("Determinant calculation for matrix size 7 x 7:")
    print("default: ")
    m = new Matrix(7, 7, true)
    time(determinantCalc(m))
    print("parallel: ")
    time(parDeterminantCalc(m))

    println
    println("Determinant calculation for matrix size 8 x 8:")
    print("default: ")
    m = new Matrix(8, 8, true)
    time(determinantCalc(m))
    print("parallel: ")
    time(parDeterminantCalc(m))

    println
    println("Determinant calculation for matrix size 9 x 9:")
    print("default: ")
    m = new Matrix(9, 9, true)
    time(determinantCalc(m))
    print("parallel: ")
    time(parDeterminantCalc(m))

    println
    println("Determinant calculation for matrix size 10 x 10:")
    print("default: ")
    m = new Matrix(10, 10, true)
    time(determinantCalc(m))
    print("parallel: ")
    time(parDeterminantCalc(m))

    println
    println("Determinant calculation for matrix size 11 x 11:")
    print("default: ")
    m = new Matrix(11, 11, true)
    time(determinantCalc(m))
    print("parallel: ")
    time(parDeterminantCalc(m))

    println
    println("Determinant calculation for matrix size 12 x 12:")
    print("default: ")
    m = new Matrix(12, 12, true)
    time(determinantCalc(m))
    print("parallel: ")
    time(parDeterminantCalc(m))
  }

  def main(args: Array[String]): Unit = {
    //mergeSortTest
    //mergeSortTimeTest

    //matrixMulTest
    //matrixMulTimeTests

    //determinantCalcTest
    determinantTimeTest
  }
}
