import annotation.tailrec

object Utils {
  def reverseList[A](list: List[A]): List[A] = {
    reverseListInternal(list, List())
  }

  @tailrec
  private def reverseListInternal[A](list: List[A], rev: List[A]): List[A] = {
    list match {
      case Nil => rev
      case head::tail => reverseListInternal(tail, head :: rev)
    }
  }
}
