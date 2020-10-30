object Task1 {
    def divide(list: List[Int]): (List[Int], List[Int]) =
        list match {
            case Nil => (Nil, Nil)
            case hd::tl => {
                val (neg, negOdd) = divide(tl)
                (( if (hd<0) hd::neg else neg ),
                ( if (hd<0) hd::negOdd else negOdd ))
            }
        }
}
