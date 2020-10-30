import scala.annotation.tailrec

class Functions {
  def length[A](list: List[A]): Int = {
    @tailrec
    def lengthHelper(list: List[A], length: Int): Int = {
      if (list == Nil) length
      else lengthHelper(list.tail, length + 1)
    }

    lengthHelper(list, 0)
  }

  def merge[A](firstList: List[A], secondList: List[A]): List[A] = {
    @tailrec
    def helper(firstList: List[A], secondList: List[A], returnList: List[A]): List[A] = {
      (firstList, secondList) match {
        case (Nil, Nil) => reverse(returnList)
        case (head :: tail, Nil) => helper(tail, secondList, head :: returnList)
        case (Nil, head :: tail) => helper(firstList, tail, head :: returnList)
        case (head :: tail, _) => helper(secondList, tail, head :: returnList)
      }
    }

    helper(firstList, secondList, Nil)
  }

  def joinLists_TailRec[A](firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] = {
    @tailrec
    def helper(firstList: List[A], secondList: List[A], thirdList: List[A], returnList: List[A]): List[A] = {
      (firstList, secondList, thirdList) match {
        case (head :: tail, _, _) => helper(tail, secondList, thirdList, head :: returnList)
        case (Nil, head :: tail, _) => helper(firstList, tail, thirdList, head :: returnList)
        case (Nil, Nil, head :: tail) => helper(firstList, secondList, tail, head :: returnList)
        case (Nil, Nil, Nil) => reverse(returnList)
      }
    }

    helper(firstList, secondList, thirdList, Nil)
  }

  def joinLists[A](firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] = {
    (firstList, secondList, thirdList) match {
      case (head :: tail, _, _) => head :: joinLists(tail, secondList, thirdList)
      case (Nil, head :: tail, _) => head :: joinLists(firstList, tail, thirdList)
      case (Nil, Nil, _) => thirdList
    }
  }


  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def helper(list: List[A], rev: List[A]): List[A] = {
      if (list == Nil) rev
      else helper(list.tail, list.head :: rev)
    }

    helper(list, Nil)
  }
}
