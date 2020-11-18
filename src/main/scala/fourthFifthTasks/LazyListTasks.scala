package fourthFifthTasks

object LazyListTasks {
  //zadanie 4 - 5 pkt
  def eachNElement[A](list: LazyList[A], k: Int, n: Int): LazyList[A] = {
    def itereachElement[A](listIter: LazyList[A], indexFirstEl: Int, till: Int): LazyList[A] = {
      listIter match {
        case LazyList() => LazyList()
        case h #:: t => if (indexFirstEl % k == 0 && till > 0) h #:: itereachElement(t, indexFirstEl + 1, till - 1) else itereachElement(t, indexFirstEl + 1, till - 1)
      }
    }

    if (k < 1 || n < 1) throw new Exception("Invalid data")
    else
      itereachElement(list, 0, n)
  }

  //zadanie 5 - 5 pkt
  def ldzialanie(list1: LazyList[Double], list2: LazyList[Double], operator: (Double, Double) => Double): LazyList[Double] = {
    (list1, list2) match {
      case (LazyList(), LazyList()) => LazyList()
      case (LazyList(), l2) => l2
      case (l1, LazyList()) => l1
      case (head1 #:: tail1, head2 #:: tail2) => operator(head1, head2) #:: ldzialanie(tail1, tail2, operator)
    }
  }
}
