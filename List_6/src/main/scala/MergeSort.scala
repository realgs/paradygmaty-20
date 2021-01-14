
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object MergeSort {

  @tailrec
  def merge(list1: List[Int], list2: List[Int], accumulator: List[Int] = List()): List[Int] =
    (list1, list2) match {
      case (Nil, _) => accumulator ++ list2
      case (_, Nil) => accumulator ++ list1
      case (headX :: xs, headY :: ys) =>
        if(headX < headY) merge(xs, list2, accumulator :+ headX)
        else merge(list1, ys, accumulator :+ headY)
  }

  def mergeSort(list: List[Int]): List[Int] =
    list match {
      case Nil => Nil
      case head :: Nil => List(head)
      case _ =>
        val (left, right) = list splitAt list.length / 2
        merge(mergeSort(left), mergeSort(right))
  }

  def mergeSortParallel(list: List[Int], depth: Int): List[Int] = {
    list match {
      case Nil => Nil
      case xs :: Nil => List(xs)
      case _ =>
        if (depth == 0){
          mergeSort(list)
        }
        else {
          val (left, right) = list.splitAt(list.length / 2)
          val futureLeft = Future(mergeSortParallel(left, depth - 1))
          val futureRight = Future(mergeSortParallel(right, depth - 1))

          merge(Await.result(futureLeft, 10.minutes), Await.result(futureRight, 10.minutes))
        }
    }
  }

  @tailrec
  def isSorted[T](list: List[T])(implicit ord: Ordering[T]): Boolean = list match {
    case Nil => true
    case x :: Nil => true
    case x :: xs => ord.lteq(x, xs.head) && isSorted(xs)
  }

}
