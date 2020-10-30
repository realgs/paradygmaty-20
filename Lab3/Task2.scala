import scala.annotation.tailrec

object Task2 {
    def count[A](list: List[A]): Int = {
        @tailrec
        def countTail(list: List[A], counter: Int): Int =
            list match {
                case Nil => counter
                case _::tl => countTail(tl, counter+1)
            }   
        countTail(list, 0)     
    }
}
