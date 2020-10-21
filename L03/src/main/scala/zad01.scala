import annotation.tailrec

object zad01 {
  def podziel(values: List[Int]): (List[Int], List[Int]) = {
    val predicates = List(
      (v: Int) => v < 0,
      (v: Int) => v < 0 && v % 2 == -1
    )
    val result = partition(values, predicates)
    (result.head, result.tail.head)
  }

  private def partition[A](values: List[A], predicates: List[A => Boolean]): List[List[A]] = {
    @tailrec
    def partitionInternal(values: List[A], predicates: List[A => Boolean], result: List[List[A]]): List[List[A]] = {
      predicates match {
        case Nil => result
        case head :: tail => partitionInternal(values, tail, applyPredicate(values, head) :: result)
      }
    }

    Utils.reverseList(partitionInternal(values, predicates, List()))
  }

  private def applyPredicate[A](values: List[A], predicate: A => Boolean): List[A] = {
    @tailrec
    def applyPredicateInternal(values: List[A], predicate: A => Boolean, result: List[A]): List[A] = {
      values match {
        case Nil => result
        case head :: tail if predicate(head) => applyPredicateInternal(tail, predicate, head :: result)
        case _ :: tail => applyPredicateInternal(tail, predicate, result)
      }
    }

    Utils.reverseList(applyPredicateInternal(values, predicate, List()))
  }
}
