import annotation.tailrec

object Utils {
  def reverseList[A](list: List[A]): List[A] = {
    @tailrec
    def reverseListInternal(list: List[A], rev: List[A]): List[A] = {
      list match {
        case Nil => rev
        case head :: tail => reverseListInternal(tail, head :: rev)
      }
    }

    reverseListInternal(list, List())
  }
}
