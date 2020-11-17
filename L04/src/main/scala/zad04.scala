// zadanie 4 (5 pkt)
object zad04 {
  def eachNElement[A](list: LazyList[A], n: Int, m: Int): LazyList[A] = {
    def helper(list: LazyList[A], toNextValue: Int, toEnd: Int): LazyList[A] = {
      if (list == LazyList() || toEnd <= 0) LazyList()
      else toNextValue match {
        case 0 => list.head #:: helper(list.tail, n - 1, toEnd - 1)
        case _ => helper(list.tail, toNextValue - 1, toEnd - 1)
      }
    }

    if (n < 1) throw new IllegalArgumentException("N must be greater or equal 1")
    helper(list, 0, m)
  }
}
