import scala.annotation.tailrec

class Functions {
  //zadanie 1
  def split(list: List[Int]): (List[Int], List[Int]) = {
    @tailrec
    def splitIter(list: List[Int], negative: List[Int], negativeAndOdd: List[Int]): (List[Int], List[Int]) = {
      if (list == Nil) (reverse(negative), reverse(negativeAndOdd))
      else if (list.head < 0) {
        if (list.head % 2 == -1) splitIter(list.tail, list.head :: negative, list.head :: negativeAndOdd)
        else splitIter(list.tail, list.head :: negative, negativeAndOdd)
      }
      else splitIter(list.tail, negative, negativeAndOdd)
    }
    splitIter(list, Nil, Nil)
  }

  //zadanie 2
  def length[A](list: List[A]): Int = {
    @tailrec
    def lengthIter (list: List[A], length: Int): Int = {
      if (list == Nil) length
      else lengthIter(list.tail, length + 1)
    }
    lengthIter(list, 0)
  }

  //zadanie 3
  def merge[A](firstList: List[A], secondList: List[A]): List[A] = {
    @tailrec
    def mergeIter(firstList: List[A], secondList: List[A], returnList: List[A]): List[A] = {
      (firstList, secondList) match {
        case (Nil, Nil) => reverse(returnList)
        case (head :: tail, Nil) => mergeIter(tail, secondList, head :: returnList)
        case (Nil, head :: tail) => mergeIter(firstList, tail, head :: returnList)
        case (head :: tail, _) => mergeIter(secondList, tail, head :: returnList)
      }
    }
    mergeIter(firstList, secondList, Nil)
  }

  //zadanie 5
  def joinLists_TailRec[A](firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] = {
    @tailrec
    def joinLists_TailRecIter(firstList: List[A], secondList: List[A], thirdList: List[A], returnList: List[A]): List[A] = {
      (firstList, secondList, thirdList) match {
        case (head :: tail, _, _) => joinLists_TailRecIter(tail, secondList, thirdList, head :: returnList)
        case (Nil, head :: tail, _) => joinLists_TailRecIter(firstList, tail, thirdList, head :: returnList)
        case (Nil, Nil, head :: tail) => joinLists_TailRecIter(firstList, secondList, tail, head :: returnList)
        case (Nil, Nil, Nil) => reverse(returnList)
      }
    }
    joinLists_TailRecIter(firstList, secondList, thirdList, Nil)
  }

  def joinLists[A](firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] = {
    (firstList, secondList, thirdList) match {
      case (head :: tail, _, _) => head :: joinLists(tail, secondList, thirdList)
      case (Nil, head :: tail, _) => head :: joinLists(firstList, tail, thirdList)
      case (Nil, Nil, _) => thirdList
    }
  }

  //own reverse function
  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def reverseIter(list: List[A], rev: List[A]): List[A] = {
      if (list == Nil) rev
      else reverseIter(list.tail, list.head :: rev)
    }
    reverseIter(list, Nil)
  }
}
