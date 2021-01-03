import lab6.sequence._
import lab6.parallel.multiplyAxB
import org.scalatest.FunSuite


class MatrixTests extends FunSuite
{
  var matrixA = new Matrix(2,2)
  var matrixB = new Matrix(2,3)
  matrixA.matrix(0)(0) = 1
  matrixA.matrix(0)(1) = -1
  matrixA.matrix(1)(0) = 2
  matrixA.matrix(1)(1) = 2
  matrixB.matrix(0)(0) = 1
  matrixB.matrix(0)(1) = 2
  matrixB.matrix(0)(2) = -1
  matrixB.matrix(1)(0) = 3
  matrixB.matrix(1)(1) = 2
  matrixB.matrix(1)(2) = 0
  var matrixC: Matrix = multiplyAxB(matrixA,matrixB)
  assert(matrixC.rows == 2)
  assert(matrixC.cols == 3)
  assert(matrixC.matrix(0)(0) == -2)
  assert(matrixC.matrix(0)(1) == 0)
  assert(matrixC.matrix(0)(2) == -1)
  assert(matrixC.matrix(1)(0) == 8)
  assert(matrixC.matrix(1)(1) == 8)
  assert(matrixC.matrix(1)(2) == -2)
}
class MatrixParallelTest extends FunSuite{
  var matrixA = new Matrix(2,2)
  var matrixB = new Matrix(2,3)
  matrixA.matrix(0)(0) = 1
  matrixA.matrix(0)(1) = -1
  matrixA.matrix(1)(0) = 2
  matrixA.matrix(1)(1) = 2
  matrixB.matrix(0)(0) = 1
  matrixB.matrix(0)(1) = 2
  matrixB.matrix(0)(2) = -1
  matrixB.matrix(1)(0) = 3
  matrixB.matrix(1)(1) = 2
  matrixB.matrix(1)(2) = 0
  var matrixC: Matrix = lab6.parallel.multiplyAxB(matrixA,matrixB)
  assert(matrixC.rows == 2)
  assert(matrixC.cols == 3)
  assert(matrixC.matrix(0)(0) == -2)
  assert(matrixC.matrix(0)(1) == 0)
  assert(matrixC.matrix(0)(2) == -1)
  assert(matrixC.matrix(1)(0) == 8)
  assert(matrixC.matrix(1)(1) == 8)
  assert(matrixC.matrix(1)(2) == -2)
}
class FindPatternParTests extends FunSuite{
  assert(lab6.parallel.findPattern(Array("index0169","index0168202","index0168211","index0168210","index0169222","index0169224" ),List("index0168","index01692"))==List("index0168202", "index0168211", "index0168210", "index0169222","index0169224"))
  assert(lab6.parallel.findPattern(Array("index0169","index0168202","index0168211","index0168210","index0169222","index0169224" ),List("index0168"))==List("index0168202", "index0168211", "index0168210"))
  assert(lab6.parallel.findPattern(Array(),Nil)==Nil)
  assert(lab6.parallel.findPattern(Array(),List("a"))==Nil)
  assert(lab6.parallel.findPattern(Array("a","b","c","ba","ca","da","dd"),List("a","d"))==List("a","ba","ca","da","dd"))
  assert(lab6.parallel.findPattern(Array("a","b","c","ba","ca","da","dd"),List("a","d","c"))==List("a","c","ba","ca","da","dd"))
  assert(lab6.parallel.findPattern(Array(123,1345,223412,98765,32456,21345,123456),List(12,56))==List(123, 223412, 32456, 123456))
}
class FindPatternSeqTests extends FunSuite{
    assert(findPattern(Array("index0169","index0168202","index0168211","index0168210","index0169222","index0169224" ),List("index0168","index01692"))==List("index0168202", "index0168211", "index0168210", "index0169222","index0169224"))
    assert(findPattern(Array("index0169","index0168202","index0168211","index0168210","index0169222","index0169224" ),List("index0168"))==List("index0168202", "index0168211", "index0168210"))
    assert(findPattern(Array(),Nil)==Nil)
    assert(findPattern(Array(),List("a"))==Nil)
    assert(findPattern(Array("a","b","c","ba","ca","da","dd"),List("a","d"))==List("a","ba","ca","da","dd"))
    assert(findPattern(Array("a","b","c","ba","ca","da","dd"),List("a","d","c"))==List("a","c","ba","ca","da","dd"))
    assert(findPattern(Array(123,1345,223412,98765,32456,21345,123456),List(12,56))==List(123, 223412, 32456, 123456))
}
class QuickSortSeqTests extends  FunSuite{
    assert(quicksort(Array(9,10,2,3,4,5,6)) sameElements Array(2,3,4,5,6,9,10))
    assert(quicksort(Array[Int]()) sameElements Array[Int]())
    assert(quicksort(Array(1,2,3,4)) sameElements Array(1,2,3,4))
    assert(quicksort(Array(7,7,6,6)) sameElements Array(6,6,7,7))
    assert(quicksort(Array(1)) sameElements Array(1))
}
class QuickSortParTests extends  FunSuite{
  assert(lab6.parallel.quicksort(Array(9,10,2,3,4,5,6)) sameElements Array(2,3,4,5,6,9,10))
  assert(lab6.parallel.quicksort(Array[Int]()) sameElements Array[Int]())
  assert(lab6.parallel.quicksort(Array(1,2,3,4)) sameElements Array(1,2,3,4))
  assert(lab6.parallel.quicksort(Array(1)) sameElements Array(1))
  assert(lab6.parallel.quicksort(Array(7,7,6,6)) sameElements Array(6,6,7,7))
}

