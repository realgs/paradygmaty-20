object zad03 {
  def polacz[A](a: List[A], b: List[A]): List[A] = {
    (a, b) match {
      case (_, Nil) => a
      case (Nil, _) => b
      case (aHead :: aTail, bHead :: bTail) => aHead :: bHead :: polacz(aTail, bTail)
    }
  }
}
