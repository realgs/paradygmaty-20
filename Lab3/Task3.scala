object Task3 {
    def zip[A](list1: List[A], list2: List[A]): List[A] =
        (list1, list2) match {
            case (Nil, _) => list2
            case (_, Nil) => list1
            case (hd1::tl1, hd2::tl2) => {
                hd1::hd2::zip(tl1,tl2)
            }
        }
}
