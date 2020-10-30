import scala.annotation.tailrec

object Task1 {
    def divide(list: List[Int]): (List[Int], List[Int]) = {
        @tailrec
        def divideTail(list: List[Int], accumNeg: List[Int], accumNegOdd: List[Int]): (List[Int], List[Int]) =    
            list match {
                case Nil => (accumNeg, accumNegOdd)
                case hd::tl => divideTail(tl,
                    ( if (hd<0) hd::accumNeg else accumNeg ),
                    ( if (hd<0 && (hd % 2 == -1)) hd::accumNegOdd else accumNegOdd ))
            }
        val (neg, negOdd) = divideTail(list, Nil, Nil)
        (Utils.reverse(neg), Utils.reverse(negOdd))
    }
}
