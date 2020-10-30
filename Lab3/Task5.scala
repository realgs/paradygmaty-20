import scala.annotation.tailrec

object Task5 {
    def join3ListsWithoutTailRec[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
        (list1, list2, list3) match {
            case (Nil, Nil, _) => list3
            case (Nil, hd::tl, _) => hd::join3ListsWithoutTailRec(list1, tl, list3)
            case (hd::tl, _, _) => hd::join3ListsWithoutTailRec(tl, list2, list3)
        }

    def join3ListsWithTailRec[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
        @tailrec
        def join3ListsTail[A](list1: List[A], list2: List[A], list3: List[A], accum: List[A]): List[A] =
            (list1, list2, list3) match {
                case (hd::tl, _, _) => join3ListsTail(tl, list2, list3, hd::accum)
                case (Nil, hd::tl, _) => join3ListsTail(list1, tl, list3, hd::accum)
                case (Nil, Nil, hd::tl) => join3ListsTail(list1, list2, tl, hd::accum)
                case (Nil, Nil, Nil) => accum
            }

        Utils.reverse(join3ListsTail(list1, list2, list3, Nil))
    }
}
