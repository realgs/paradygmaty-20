import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}



object MergeSort {
  @tailrec
  def reverse(list:List[Int], tail:List[Int]):List[Int]=
    if(list == Nil) tail
    else reverse(list.tail,list.head::tail)

  def divide(list: List[Int]): (List[Int], List[Int]) = {
    @tailrec
    def helper(list: List[Int], tail: List[Int], count: Int):(List[Int], List[Int]) ={
      if(count == 0) (reverse(list,Nil),tail)
      else
        helper(tail.head::list,tail.tail,count -1)
    }
    val m = list.length/2
    helper(Nil,list,m)
  }

  @tailrec
  def merge(left: List[Int], right: List[Int], newList: List[Int]):List[Int] =
    (left, right) match {
      case (left, Nil) => reverse(newList,left)
      case (Nil, right) => reverse(newList,right)
      case (leftHead :: leftTail, rightHead :: rightTail) =>
        if (leftHead < rightHead) merge(leftTail, right,leftHead::newList)
        else merge(left, rightTail, rightHead::newList)
    }

  def mergeSort(list: List[Int]):List[Int] ={
    if(list.length == 1)
      list
    else if(list.length == 2){
      if(list.head > list.tail.head){
        list.tail.head::list.head::Nil
      }
      else
        list
    }
    else{
      val (left, right) = divide(list)
      val mergedLeft = mergeSort(left)
      val mergedRight = mergeSort(right)
      merge(mergedLeft, mergedRight,Nil)
    }
  }

  def mergeSortParallel(list: List[Int]):List[Int] ={
    if(list.length == 1)
      list
    else if(list.length == 2){
      if(list.head > list.tail.head){
        list.tail.head::list.head::Nil
      }
      else
        list
    }
    else{
      val (left, right) = divide(list)
      val mergedLeftP = Future{mergeSort(left)}
      val mergedRightP = Future{mergeSort(right)}
      merge(Await.result(mergedLeftP,Duration.Inf),Await.result(mergedRightP,Duration.Inf),Nil)
    }
  }
}