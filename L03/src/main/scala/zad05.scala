import annotation.tailrec

object zad05 {
  def joinLists[A](a: List[A], b: List[A], c: List[A]): List[A] = {
    flatten(List(a, b, c))
  }

  private def flatten[A](list: List[List[A]]): List[A] = {
    list match {
      case Nil => Nil
      case head :: tail => Utils.appendList(head, flatten(tail))
    }
  }

  def joinListsTail[A](a: List[A], b: List[A], c: List[A]): List[A] = {
    flattenTail(List(a, b, c))
  }

  private def flattenTail[A](list: List[List[A]]): List[A] = {
    @tailrec
    def flattenInternal(list: List[List[A]], result: List[A]): List[A] = {
      list match {
        case Nil => result
        case head :: tail => flattenInternal(tail, Utils.appendList(head, result))
      }
    }

    flattenInternal(Utils.reverseList(list), List())
  }
}
