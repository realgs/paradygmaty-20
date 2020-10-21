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

  def appendList[A](a: List[A], b: List[A]): List[A] = {
    (a, b) match {
      case (_, Nil) => a
      case (Nil, _) => b
      case (head :: tail, _) => head :: appendList(tail, b)
    }
  }
}
