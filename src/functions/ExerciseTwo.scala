package functions

import scala.annotation.tailrec

object ExerciseTwo {
  def listSize[A](list: List[A]): Int = {
    @tailrec
    def innerListSize(list: List[A], accum: Int): Int =
      list match {
        case Nil => accum
        case _ :: tail => innerListSize(tail, accum + 1)
      }
    innerListSize(list, 0)
  }

}
