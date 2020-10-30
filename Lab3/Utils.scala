import scala.annotation.tailrec

object Utils {
    def reverse[A](list: List[A]): List[A] = {
        @tailrec
        def reverseTail[A](list: List[A], reversed: List[A]): List[A] =
            list match {
                case Nil => reversed
                case hd::tl => reverseTail(tl, hd::reversed)
            }
        reverseTail(list, Nil)
    }
}
