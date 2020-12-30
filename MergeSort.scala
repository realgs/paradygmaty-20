package List6

import List6.Parallel.parallel
import math.Ordering

object MergeSort {

  private def merge[T](left: List[T], right: List[T])(order: Ordering[T]): List[T] =
    (left, right) match {
      case (Nil, _) => right
      case (_, Nil) => left
      case (lHead :: lTail, rHead :: rTail) =>
        if (order.lt(lHead, rHead))
          lHead :: merge(lTail, right)(order)
        else
          rHead :: merge(left, rTail)(order)
    }

  def sort[T](list: List[T])(implicit order: Ordering[T]): List[T] = {
    val mid = list.length / 2
    if (mid == 0) list
    else {
      val (left, right) = list splitAt mid
      merge(sort(left), sort(right))(order)
    }
  }

  def parSort[T](list: List[T])(implicit order: Ordering[T]): List[T] = {
    val mid = list.length / 2
    if (mid == 0) list
    else {
      val (left, right) = list.splitAt(mid)
      val (leftList, rightList) = parallel(sort(left), sort(right))
      merge(leftList, rightList)(order)
    }
  }
}
