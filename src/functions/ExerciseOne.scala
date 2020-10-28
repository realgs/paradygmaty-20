package functions

import utilities.UtilityFunctions.reverseList

import scala.annotation.tailrec

object ExerciseOne {
  def createTwoLists(list: List[Int]): (List[Int], List[Int]) = {
    @tailrec
    def createTwoListsTail(sharedList: List[Int], firstList: List[Int], secondList: List[Int]): (List[Int], List[Int]) =
      sharedList match {
        case Nil => (reverseList(firstList), reverseList(secondList))
        case head :: tail => createTwoListsTail(tail, if (head < 0) head :: firstList else firstList, if (head < 0 && head % 2 != 0) head :: secondList else secondList)
      }

    createTwoListsTail(list, Nil, Nil)
  }

}
