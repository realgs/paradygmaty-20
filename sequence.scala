package lab6

import scala.annotation.tailrec
import scala.util.Random

object sequence {
  //sequence matrix multiplication
  def multiplyAxB(matrixA: Matrix, matrixB: Matrix): Matrix = {
    if (matrixA.cols == matrixB.rows) {
      val product = new Matrix(matrixA.rows, matrixB.cols)
      for (y <- 0 until product.rows) {
        for (x <- 0 until product.cols) {
          var result = 0
          for (i <- 0 until matrixA.rows)
            {
              result += matrixA.matrix(y)(i) * matrixB.matrix(i)(x)
            }
            product.matrix(y)(x) = result
        }
      }
      product
    }
    else new Matrix(0,0)
  }
  class Matrix(val rows: Int,val cols: Int) {
    var matrix: Array[Array[Int]] = Array.ofDim[Int](rows, cols)
  }
  //findPattern
  @tailrec
  def reverse[A](zs:List[A],ys:List[A]): List[A]=
    zs match {
      case Nil=>ys
      case h::t=>reverse(t,h::ys)
    }
  def myContains(elem:String,pat:String): Boolean={
    @tailrec
    def contIter(ls:List[Char],ps:List[Char]): Boolean={
      (ls,ps) match {
        case (_,Nil)=>true
        case (Nil,_)=>false
        case (hl::tl,hp::tp)=> if(hl==hp)contIter(tl,tp)
        else contIter(tl,pat.toList)
      }
    }
    contIter(elem.toList,pat.toList)
  }
  @tailrec
  def checkElem[A](elem:A, patterns:List[A]): Boolean=
    patterns match{
      case Nil=> false
      case h::t=>myContains(elem.toString,h.toString) || checkElem(elem,t)
    }
  def findPattern[A] (xs:Array[A],patterns:List[A]): List[A]= {
    var ys = List[A]()
    for(i<-xs.indices)
    {
        if(checkElem(xs(xs.length-i-1),patterns)) ys = xs(xs.length-i-1)::ys
    }
    ys
  }

  //quicksort
  def quicksort(arr: Array[Int]): Array[Int] = {
    def quick(startIndex: Int, endIndex: Int): Unit = {
        if(startIndex+1 < endIndex){
          val part = partition(startIndex,endIndex)
          quick(startIndex, part)
          quick(part+1, endIndex)
        }
    }
    def partition(from: Int,to : Int): Int = {
      val rnd = from+Random.nextInt(to-from)
      swap(from,rnd)
      val pivotVal = arr(from)
      var idxBigger = from + 1
      var idxLower = to - 1
      do{
        while(idxBigger <= idxLower && arr(idxBigger) <= pivotVal)idxBigger+=1
        while(arr(idxLower) > pivotVal)idxLower-=1
        if (idxBigger < idxLower) swap(idxBigger, idxLower)
      } while(idxBigger < idxLower)
      swap(idxLower, from)
      idxLower
    }
    def swap(left: Int, right: Int): Unit = {
        if(left != right){
           val temp = arr(left)
           arr(left) = arr(right)
           arr(right) = temp
        }
    }
    quick(0,arr.length)
    arr
  }
}

