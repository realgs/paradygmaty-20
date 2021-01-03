import scala.collection.parallel._
import Benchmarking._

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.Random
import scala.concurrent.ExecutionContext.Implicits.global

object Main {
  def mergeSort[A](order: A=>A=>Boolean)(toSortList: List[A]): List[A] = {
    def forEachPair[B](predicate: (B,B) => B)(list: List[B])(lastElem: B): List[B] = {
      list match {
        case Nil => Nil
        case List(x) => List(predicate(x, lastElem))
        case hd1::hd2::tl => predicate(hd1, hd2) :: forEachPair(predicate)(tl)(lastElem)
      }
    }

    def merge[A](order: A=>A=>Boolean)(list1: List[A])(list2: List[A]): List[A] = {
      (list1, list2) match {
        case (Nil, _) => list2
        case (_, Nil) => list1
        case (hd1::tl1, hd2::tl2) => if (order(hd1)(hd2)) hd1::merge(order)(tl1)(list2)
        else hd2::merge(order)(list1)(tl2)
      }
    }

    def mergeSortRec(wrappersList: List[List[A]]): List[A] = {
      wrappersList match {
        case List(x) => x
        case _ => mergeSortRec(forEachPair[List[A]]((b1,b2) => merge(order)(b1)(b2))(wrappersList)(Nil))
      }
    }

    mergeSortRec(toSortList.map(x =>List(x)))
  }

  def mergeSortParallel[A](order: A=>A=>Boolean)(toSortList: List[A]): List[A] = {
    def merge[A](f: A => A => Boolean)(left: List[A], right: List[A]): List[A] =
    {
      (left, right) match
      {
        case (left, Nil) => left
        case (Nil, right) => right
        case(leftHead :: leftTail, rightHead :: rightTail) =>
          if (f(leftHead)(rightHead)) leftHead :: merge(f)(leftTail, right)
          else rightHead :: merge(f)(left, rightTail)
      }
    }

    toSortList match
    {
      case Nil => Nil
      case List(x) => List(x)
      case _ => val (left, right) = toSortList.splitAt(toSortList.length / 2)
        merge(order)(mergeSort(order)(left), mergeSort(order)(right))
    }
  }

  def main(args: Array[String]): Unit = {
    def quicksortBenchmark(numberOfTestSets: Int)(repeatsPertestSet: Int): List[AlghoritmStats] = compareBenchmarkArray(List(Quicksort.quicksort _, Quicksort.quicksortParallel _))((n: Int) => Array.fill(n)(Random.nextInt()))(100000)(numberOfTestSets)(repeatsPertestSet)
    var result2 = quicksortBenchmark(1)(100).flatten
    println(result2)
    println(s"mean: ${result2(0).mean}")
    println(s"variance: ${result2(0).variance}")
    println(s"mean: ${result2(1).mean}")
    println(s"variance: ${result2(1).variance}")
    println()
  }
}

object Quicksort
{
  private def swap[A](array: Array[A])(i: Int)(j: Int): Unit =
  {
    val aux = array(i)
    array(i) = array(j)
    array(j) = aux
  }

  private def partition(array: Array[Int])(left: Int)(right: Int): (Int, Int) =
  {
    var i = left
    var j = right
    val pivot = array((i + j) / 2)
    while (i <= j)
    {
      while (array(i) < pivot) i += 1
      while (array(j) > pivot) j -= 1
      if (i <= j)
      {
        swap(array)(i)(j)
        i += 1
        j -= 1
      }
    }
    (i, j)
  }

  private def quick(array: Array[Int])(left: Int)(right: Int): Unit = {
  {
    if (left < right)
    {
      val (i, j) = partition(array)(left)(right)
      if (j - left < right - i)
      {
        quick(array)(left)(j)
        quick(array)(i)(right)
      }
      else
      {
        quick(array)(i)(right)
        quick(array)(left)(j)
      }
    }
  }

  }

  private def quickParallel(array: Array[Int])(left: Int)(right: Int)(depth: Int): Unit =
  {
    if(depth > 0)
    {
      if (left < right)
      {
        val (i, j) = partition(array)(left)(right)
        if (j - left < right - i) {
          val futureLeft = Future(quickParallel(array)(left)(j)(depth - 1))
          val futureRight = Future(quickParallel(array)(i)(right)(depth - 1))
          Await.result(futureLeft, 1000.seconds)
          Await.result(futureRight, 1000.seconds)
        }
        else {
          val futureRight = Future(quickParallel(array)(i)(right)(depth - 1))
          val futureLeft = Future(quickParallel(array)(left)(j)(depth - 1))
          Await.result(futureLeft, 1000.seconds)
          Await.result(futureRight, 1000.seconds)
        }
      }
    }
    else quick(array)(left)(right)
  }

  def quicksort(array: Array[Int]): Unit =
  {
    quick(array)(0)(array.length - 1)
  }

  def quicksortParallel(array: Array[Int]): Unit =
  {
    quickParallel(array)(0)(array.length - 1)(4)
  }

}


