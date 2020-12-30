package List6

import List6.Parallel.parallel
import scala.annotation.tailrec

object SumOfAllPrevElem {

  def sumOfAllPrevElem(array: Array[Int]): Int = {
    @tailrec
    def sumOfAllPrevElemInner(array: Array[Int], result: Int): Int = {
      array match {
        case Array() => result
        case _  => sumOfAllPrevElemInner(array.tail, result + array.head)
      }
    }
    sumOfAllPrevElemInner(array, 0)
  }

  def parSumOfAllPrevElem(array: Array[Int]): Int = {
    val mid = array.size / 2
    val (array1, array2) = array.splitAt(mid)
    val (left, right) = parallel(sumOfAllPrevElem(array1), sumOfAllPrevElem(array2))
    left + right
  }

}
