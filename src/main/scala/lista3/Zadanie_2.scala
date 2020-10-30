package lista3

import scala.annotation.tailrec

object Zadanie_2 {
  def dlugosc[A](list: List[A]): Int =
    if (list == Nil) 0
    else 1 + dlugosc(list.tail)

  //tailRec
  def dlugoscTail[A](list: List[A]): Int = {
    @tailrec
    def dlugoscIter[A](list: List[A], accum: Int): Int = {
      if (list == Nil) accum
      else dlugoscIter(list.tail, accum + 1)
    }
    dlugoscIter(list, 0)
  }
}
