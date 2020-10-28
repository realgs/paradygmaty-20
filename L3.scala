import scala.annotation.tailrec

class L3 {
  // 1)
  def splitTable(xs:List[Int]):(List[Int],List[Int]) ={
    def splitNegative(xs: List[Int]): List[Int] ={
      if (xs == Nil) Nil
      else if (xs.head < 0) xs.head :: splitNegative(xs.tail)
      else splitNegative(xs.tail)
    }

    def splitOddNegative(xs: List[Int]): List[Int] ={
      if (xs == Nil) Nil
      else if (xs.head < 0 && xs.head % 2 != 0) xs.head :: splitOddNegative(xs.tail)
      else splitOddNegative(xs.tail)
    }

    (splitNegative(xs), splitOddNegative(xs))
  }

  // 2)
  def length[A](xs:List[A]):Int ={
    @tailrec
    def lengthHelper(length:Int, xs:List[A]):Int ={
      if(xs==Nil) length
      else lengthHelper(length+1,xs.tail)
    }
    lengthHelper(0,xs)
  }

  // 3)
  def mergeLists[A](xs1:List[A],xs2:List[A]):List[A] ={
    (xs1,xs2) match {
      case (Nil,_) => xs2
      case(_,Nil) => xs1
      case(_,_) => xs1.head :: xs2.head :: mergeLists (xs1.tail, xs2.tail)
    }
  }
}
