package utilities

import scala.annotation.tailrec

object UtilityFunctions {
  def reverseList[A](list: List[A]): List[A] = {
    @tailrec
    def reverseTail(list: List[A], accum: List[A]): List[A] = {
      list match {
        case Nil => accum
        case head :: tail => reverseTail(tail, head :: accum)
      }
    }

    reverseTail(list, Nil)
  }
}
