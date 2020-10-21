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
    Utils.reverseList(partitionInternal(values, predicates, List()))
  }

  @tailrec
  private def partitionInternal[A](values: List[A], predicates: List[A => Boolean], result: List[List[A]]): List[List[A]] = {
    predicates match {
      case Nil => result
      case head :: tail => partitionInternal(values, tail, applyPredicate(values, head) :: result)
    }
  }

  private def applyPredicate[A](values: List[A], predicate: A => Boolean): List[A] = {
    Utils.reverseList(applyPredicateInternal(values, predicate, List()))
  }

  @tailrec
  private def applyPredicateInternal[A](values: List[A], predicate: A => Boolean, result: List[A]): List[A] = {
    values match {
      case Nil => result
      case head :: tail if predicate(head) => applyPredicateInternal(tail, predicate, head :: result)
      case head :: tail => applyPredicateInternal(tail, predicate, result)
    }
  }
}
