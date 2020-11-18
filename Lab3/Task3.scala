object Task3 {
    // złożoność czasowa liniowa O(n), gdzie n - długość krótszej listy wejściowej
    // złożoność pamięciowa liniowa O(n), gdzie n - długość krótszej listy wejściowej
    def zip[A](list1: List[A], list2: List[A]): List[A] =
        (list1, list2) match {
            case (Nil, _) => list2
            case (_, Nil) => list1
            case (hd1::tl1, hd2::tl2) => {
                hd1::hd2::zip(tl1,tl2)
            }
        }
}
