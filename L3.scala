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

  // 5
  //rekursja nieogonowa
  def joinListsRec[A](xs1:List[A],xs2:List[A],xs3:List[A]):List[A] ={
    (xs1,xs2,xs3)match {
    case (head::tail,_,_) => head :: joinListsRec (tail, xs2, xs3)
    case (Nil,head::tail,_) => head :: joinListsRec (Nil, tail, xs3)
    case (Nil,Nil,head::tail) => head :: joinListsRec (Nil, Nil, tail)
    case (Nil,Nil,Nil) => Nil
    }
  }

  //rekursja ogonowa
  def joinListsTailRec[A](xs1:List[A],xs2:List[A],xs3:List[A]):List[A] ={
    @tailrec
    def joinListHelper[A](xs1:List[A], xs2:List[A], xs3:List[A], xs:List[A]):List[A] ={
      (xs1,xs2,xs3)match {
        case (head::tail,_,_) => joinListHelper(tail,xs2,xs3,head::xs)
        case (Nil,head::tail,_) => joinListHelper(Nil,tail,xs3,head::xs)
        case (Nil,Nil,head::tail) => joinListHelper(Nil,Nil,tail,head::xs)
        case (Nil,Nil,Nil) => xs
      }
    }
    @tailrec
    def reverse[A](xs1:List[A], xs:List[A]):List[A] ={
      if(xs1==Nil) xs
      else reverse(xs1.tail,xs1.head::xs)
    }
    reverse(joinListHelper(xs1,xs2,xs3,Nil),Nil)
  }
}
