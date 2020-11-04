package functions

import scala.annotation.tailrec
import utilities.UtilityFunctions.reverseList

object ExerciseFive {
  def joinLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
    (list1, list2, list3) match {
      case (Nil, Nil, Nil) => Nil
      case (h1 :: t1, _, _) => h1 :: joinLists(t1, list2, list3)
      case (_, h2 :: t2, _) => h2 :: joinLists(list1, t2, list3)
      case (_, _, h3 :: t3) => h3 :: joinLists(list1, list2, t3)
    }
  }
  // n = (size of first list) + (size of second list) + (size of third list)
  // time complexity = O(n)
  // space complexity = O(n)

  def joinListsWithTailRecursion[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
    @tailrec
    def joinListsTail(accum: List[A], list1: List[A], list2: List[A], list3: List[A]): List[A] = {
      (list1, list2, list3) match {
        case (Nil, Nil, Nil) => accum
        case (_, _, h3 :: t3) => joinListsTail(h3 :: accum, list1, list2, t3)
        case (_, h2 :: t2, _) => joinListsTail(h2 :: accum, list1, t2, list3)
        case (h1 :: t1, _, _) => joinListsTail(h1 :: accum, t1, list2, list3)
      }
    }
    joinListsTail(Nil, reverseList(list1), reverseList(list2), reverseList(list3))
  }
  // n = (size of first list) + (size of second list) + (size of third list)
  // time complexity = O(n)
  // space complexity = O(1) because of tail recursion

}
