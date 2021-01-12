package scala.exercises

object ExerciseFive {
  //Zad5 (5pkt)
  def ldzialanie[A](llist1: LazyList[A], llist2: LazyList[A], dzialanie: (A, A) => A): LazyList[A] = {
    (llist1, llist2) match {
      case (LazyList(), LazyList()) => LazyList()
      case (LazyList(), llist2) => llist2
      case (llist1, LazyList()) => llist1
      case (l1head #:: l1tail, l2head #:: l2tail) => dzialanie(l1head, l2head) #:: ldzialanie(l1tail, l2tail, dzialanie)
    }
  }
}
