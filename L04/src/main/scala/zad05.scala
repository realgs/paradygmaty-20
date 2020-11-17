// zadanie 5 (5 pkt)
object zad05 {
  def ldzialanie[A](a: LazyList[A], b: LazyList[A], fun: (A, A) => A): LazyList[A] = {
    (a, b) match {
      case (LazyList(), _) => b
      case (_, LazyList()) => a
      case (ah #:: at, bh #:: bt) => fun(ah, bh) #:: ldzialanie(at, bt, fun)
    }
  }
}
