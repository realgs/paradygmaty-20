package scala.exercises

object ExerciseFour {
  //Zad4 (5pkt)
  def eachNElement[A](llist: LazyList[A], n: Int, m: Int): LazyList[A] = {
    def helper(lazyList: LazyList[A], nn: Int, mm: Int): LazyList[A] = {
      if (mm > 0)
      lazyList match {
        case head #:: tail => if (nn % n == 0) head #:: helper(tail, nn + 1, mm - 1) else helper(tail, nn + 1, mm - 1)
        case LazyList() => LazyList()
      }
      else LazyList()
    }
    if (n > 0 && m >= 0) helper(llist, n, m) else throw new IllegalArgumentException
  }
}
