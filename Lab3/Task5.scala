import scala.annotation.tailrec

object Task5 {
    def join3Lists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
        (list1, list2, list3) match {
            case (Nil, Nil, _) => list3
            case (Nil, hd::tl, _) => hd::join3Lists(list1, tl, list3)
            case (hd::tl, _, _) => hd::join3Lists(tl, list2, list3)
        }

    @tailrec
    def join3ListsTail[A](list1: List[A], list2: List[A], list3: List[A], accum: List[A]): List[A] =
        
}