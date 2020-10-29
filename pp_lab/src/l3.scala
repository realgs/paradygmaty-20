import scala.annotation.tailrec

object l3 {

  //ZADANIE 1
  def into2(xs: List[Double]): (List[Double], List[Double]) = {
    if (xs == Nil) (Nil, Nil)
    else if (xs.head < 0) {
      if (xs.head % 2 == 1 || xs.head % 2 == -1) (xs.head :: into2(xs.tail)._1, xs.head :: into2(xs.tail)._2)
      else (xs.head :: into2(xs.tail)._1, into2(xs.tail)._2)
    } else into2(xs.tail)
  }

  //ZADANIE 2
  def myLength[A](xs: List[A]): Int = {
    @tailrec
    def myLengthRec[A](xs: List[A], length: Int): Int = {
      if (xs == Nil) length
      else myLengthRec(xs.tail, 1 + length)
    }

    myLengthRec(xs, 0)
  }

  //ZADANIE 3
  def combine2[A](xs1: List[A], xs2: List[A]): List[A] =
    if (xs1 == Nil)
      if (xs2 == Nil) Nil
      else xs2.head :: combine2(xs1, xs2.tail)
    else xs1.head :: combine2(xs2, xs1.tail)

  //ZADANIE 4

  //ZADANIE 5
  /*def combine3[A](xs1: List[A], xs2: List[A], xs3: List[A]): List[A] =
    if (xs1 == Nil)
      if (xs2 == Nil)
        if (xs3 == Nil) Nil
        else xs3.head :: combine3(xs1, xs2, xs3.tail)
      else xs2.head :: combine3(xs3, xs1, xs2.tail)
    else xs1.head :: combine3(xs2, xs3, xs1.tail)(*/

  def combine3[A](xs1: List[A], xs2: List[A], xs3: List[A]): List[A] =
    if (xs1 == Nil)
      if (xs2 == Nil)
        if (xs3 == Nil) Nil
        else xs3.head :: combine3(xs1, xs2, xs3.tail)
      else xs2.head :: combine3(xs1, xs2.tail, xs3)
    else xs1.head :: combine3(xs1.tail, xs2, xs3)

  def combine3Tail[A](xs1: List[A], xs2: List[A], xs3: List[A]): List[A] = {
    @tailrec
    def combine3Rec[A](xs1: List[A], xs2: List[A], xs3: List[A], curr: List[A]): List[A] = (xs1, xs2, xs3) match {
      case (Nil, Nil, Nil) => curr
      case (Nil, Nil, _) => combine3Rec(xs1, xs2, xs3.tail, xs3.head :: curr)
      case (Nil, _, _) => combine3Rec(xs1, xs2.tail, xs3, xs2.head :: curr)
      case _ => combine3Rec(xs1.tail, xs2, xs3, xs1.head :: curr)
    }
    myReverse(combine3Rec(xs1, xs2, xs3, List()))
  }

  def myReverse[A](xs: List[A]): List[A] =
    if (xs == Nil) Nil
    else myReverse(xs.tail) appended xs.head
}
