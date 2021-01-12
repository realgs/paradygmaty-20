object Tasks45 {
    // Zad4 (5pkt)
    def eachNElement[A](llist: LazyList[A])(n: Int)(m: Int): LazyList[A] = {
        def eachNElementCountdown(llist2: LazyList[A])(k: Int)(m2: Int): LazyList[A] = {
            llist2 match {
                case LazyList() => LazyList()
                case hd #:: tl => {
                    if (m2 < 0) LazyList()
                    else if (k == 0) hd #:: eachNElementCountdown(tl)(n-1)(m2-1)
                    else eachNElementCountdown(tl)(k - 1)(m2-1)
                }
            }
        }
        if (n < 1) throw new Exception("n cannot be less than 1")
        if (m < 0) throw new Exception("m cannot be negative")
        eachNElementCountdown(llist)(0)(m)
    }
    // Zad5 (5pkt)
    def ldzialanie[A](operation: (A, A) => A)(llist1: LazyList[A], llist2: LazyList[A]): LazyList[A] = {
        (llist1, llist2) match {
            case (hd1 #:: tl1, hd2 #:: tl2) => operation(hd1, hd2) #:: ldzialanie(operation)(tl1, tl2)
            case _ => LazyList()
        }
    }
}
