import annotation.tailrec

object zad02 {
  def dlugosc[A](list: List[A]): Int = {
    @tailrec
    def dlugoscInternal(list: List[A], result: Int): Int = {
      list match {
        case Nil => result
        case head :: tail => dlugoscInternal(tail, result + 1)
      }
    }

    dlugoscInternal(list, 0)
  }
}
