import scala.annotation.tailrec

object Task2 {
    // złożoność czasowa liniowa O(n), gdzie n - długość listy wejściowej
    // złożoność pamięciowa stała O(1)
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
