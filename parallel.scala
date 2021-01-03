package lab6

import sequence._

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.reflect.ClassTag
import scala.util.Random

object parallel {
  def multiplyAxB(matrixA: Matrix, matrixB: Matrix): Matrix = {
    if (matrixA.cols == matrixB.rows){
      val product = new Matrix(matrixA.rows, matrixB.cols)
      def countCols(start: Int, end: Int): Unit = {
        for (r <- start until end) {
          for (c <- 0 until matrixB.cols) {
            count(r, c)
          }
        }
      }
      def count(row: Int, col: Int): Unit = {
        var result = 0
        for (i <- matrixA.matrix.indices) {
          result += matrixA.matrix(row)(i) * matrixB.matrix(i)(col)
        }
        product.matrix(row)(col) = result
      }
      val futures = new Array[Future[Unit]](4)
      val divider = product.cols / futures.length
      for (i <- futures.indices) {
        if (i == futures.length - 1) futures(i) = Future {
          countCols(i * divider, product.cols)
        }
        else futures(i) = Future {
          countCols(i * divider, (i + 1) * divider)
        }
      }
      for (i <- futures.indices) {
        Await.ready(futures(i), Duration.Inf)
      }
      product
    }
    else new Matrix(0,0)
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
  def findPattern[A: ClassTag](xs:Array[A], patterns:List[A]): List[A]={
      val futures = new Array[Future[List[A]]](8)
      val divider = xs.length/futures.length
      for(i<- futures.indices){
          if(i == futures.length -1) futures(i) = Future{
              findPatternInner(xs,i*divider,xs.length,patterns)
          }
          else futures(i)= Future{
              findPatternInner(xs,i*divider,(i+1)*divider,patterns)
        }
      }
      val arrayOfLists = new Array[List[A]](futures.length)
      for (i <- futures.indices)
      {
          arrayOfLists(i) = Await.result(futures(i), Duration.Inf)
      }
      var ys = List[A]()
      @tailrec
      def connect[A](xs: List[A], zs: List[A]): List[A]={
        xs match {
          case Nil => zs
          case h::t=>connect(t, h::zs)
        }
      }
      for(j<-futures.indices){
          ys = connect(arrayOfLists(futures.length - j-1),ys)
      }
      ys
  }
  def findPatternInner[A] (xs:Array[A],start: Int,end: Int,patterns:List[A]): List[A]= {
    var ys = List[A]()
    for(i<-start until end)
    {
        if(checkElem(xs(i),patterns)) ys = xs(i)::ys
    }
    ys
  }
  //quicksort
  def quicksort(arr: Array[Int]): Array[Int] = {
    def quick(startIndex: Int, endIndex: Int): Unit = {
      if(startIndex+1 < endIndex)
      {
        val part = partition(startIndex,endIndex)
        Future {quick(startIndex, part)}
        Future {quick(part+1, endIndex)}
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
      if (left != right) {
        val temp = arr(left)
        arr(left) = arr(right)
        arr(right) = temp
      }
    }
    quick(0,arr.length)
    arr
  }
}

