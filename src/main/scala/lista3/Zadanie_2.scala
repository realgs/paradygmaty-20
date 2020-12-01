package lista3

import scala.annotation.tailrec

object Zadanie_2 {
  // O(n) - złożoność obliczeniowa
  // O(1) - złożonośc pamięciowa

  def lengthTail[A](list: List[A]): Int = {
    @tailrec
    def lengthTailIter[A](list: List[A], accum: Int): Int = {
      if (list == Nil) accum
      else lengthTailIter(list.tail, accum + 1)
    }
    lengthTailIter(list, 0)
  }
}
