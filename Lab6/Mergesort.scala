import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Mergesort {
  def mergeSortParallel[A](order: A=>A=>Boolean)(depth: Int)(toSort: List[A]): List[A] = {
    if (depth > 0) {
      toSort match {
        case _ :: _ :: _ => {
          val (left, right) = toSort.splitAt(toSort.length / 2)
          val future = Future(mergeSortParallel(order)(depth-1)(left))
          val rightSorted = mergeSort(order)(right)
          val leftSorted = Await.result(future, 10.minutes)
          merge(order)(leftSorted, rightSorted)
        }
        case toSort => toSort
      }
    } else mergeSort(order)(toSort)
  }

  private def merge[A](order: A=>A=>Boolean)(list1: List[A], list2: List[A]): List[A] = {
    (list1, list2) match {
      case (Nil, list2) => list2
      case (list1, Nil) => list1
      case (hd1::tl1, hd2::tl2) => if (order(hd1)(hd2)) hd1::merge(order)(tl1, list2)
                                   else hd2::merge(order)(list1, tl2)
    }
  }

  def mergeSort[A](order: A => A => Boolean)(xs: List[A]): List[A] = {
    xs match {
      case _ :: _ :: _ => {
        val (left, right) = xs.splitAt(xs.length / 2)
        merge(order)(mergeSort(order)(left), mergeSort(order)(right))
      }
      case xs => xs
    }
  }
}
