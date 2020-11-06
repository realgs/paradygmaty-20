import scala.annotation.tailrec

object l3 {

  //ZADANIE 1
  //Złożoność obliczeniowa O(xs), pamięciowa O(xs)
  def into2(xs: List[Double]): (List[Double], List[Double]) = {
    if (xs == Nil) (Nil, Nil)
    else if (xs.head < 0) {
      if (xs.head % 2 == 1 || xs.head % 2 == -1) (xs.head :: into2(xs.tail)._1, xs.head :: into2(xs.tail)._2)
      else (xs.head :: into2(xs.tail)._1, into2(xs.tail)._2)
    } else into2(xs.tail)
  }

  //ZADANIE 2
  //Złożonoś obliczeniowa O(xs), pamięciowa O(1)
  def myLength[A](xs: List[A]): Int = {
    @tailrec
    def myLengthRec(xs: List[A], length: Int): Int = {
      if (xs == Nil) length
      else myLengthRec(xs.tail, 1 + length)
    }

    myLengthRec(xs, 0)
  }

  //ZADANIE 3
  //Złożoność obliczeniowa O(n) (gdzie n - dłuższa z list xs1, xs2)
  //Złożoność pamięciowa O(1)
  def combine2[A](xs1: List[A], xs2: List[A]): List[A] = {
    @tailrec
    def combine2Rec(xs1: List[A], xs2: List[A], res: List[A]): List[A] = {
      (xs1, xs2) match {
        case (Nil, Nil) => myReverse(res)
        case (h1 :: t1, h2 :: t2) => combine2Rec(t1, t2, h2 :: h1 :: res)
        case (Nil, h :: t) => combine2Rec(xs1, t, h :: res)
        case (h :: t, Nil) => combine2Rec(t, xs2, h :: res)
      }
    }
    combine2Rec(xs1, xs2, List())
  }


  //ZADANIE 4
  //Złożoność obliczeniowa O(xs*n) (gdzie n wynika ze złożoności metody myContains i oznacza najdłuższy ze stringów z listy lub x)
  //Złożoność pamięciowa O(xs)
  def find(xs: List[String], x: String): List[String] = {
    if (xs == Nil) xs
    else if (myContains(xs.head, x)) xs.head :: find(xs.tail, x)
    else find(xs.tail, x)
  }

  //Złożoność obliczeniowa O(xs*n) (gdzie n wynika ze złożoności metody myContains i oznacza najdłuższy ze stringów z listy lub x)
  //Złożoność pamięciowa O(1)
  def findTail(xs: List[String], x: String): List[String] = {
    @tailrec
    def findTailRec(xs: List[String], x: String, res: List[String]): List[String] = {
      if (xs == Nil) res
      else if (myContains(xs.head, x)) findTailRec(xs.tail, x, xs.head :: res)
      else findTailRec(xs.tail, x, res)
    }
    findTailRec(xs, x, List())
  }

  //Złożoność obliczeniowa O(xs*ys*n) (gdzie n wynika ze złożoności metody myContains i oznacza najdłuższy ze stringów w którejś z list)
  //Złożoność pamięciowa O(xs)
  def findMultiple(xs: List[String], ys: List[String]): List[String] = {
    var flag = false
    if (xs == Nil) return Nil
    else for (str <- ys) if (myContains(xs.head, str)) flag = true
    if (flag) xs.head :: findMultiple(xs.tail, ys)
    else findMultiple(xs.tail, ys)
  }

  //Złożoność obliczeniowa O(xs*ys*n) (gdzie n wynika ze złożoności metody myContains i oznacza najdłuższy ze stringów w którejś z list)
  //Złożoność pamięciowa O(1)
  def findMultipleTail(xs: List[String], ys: List[String]): List[String] = {
    @tailrec
    def findMultipleRec(xs: List[String], ys: List[String], res: List[String], ys1: List[String]): List[String] =
      (xs, ys1) match {
        case (Nil, _) => myReverse(res)
        case (_ :: tl, Nil) => findMultipleRec(tl, ys, res, ys)
        case (h1 :: t1, h2 :: t2) => if (myContains(h1, h2)) findMultipleRec(t1, ys, h1 :: res, ys)
        else findMultipleRec(xs, ys, res, t2)
      }
    findMultipleRec(xs, ys, List(), ys)
  }

  //ZADANIE 5
  //Złożoność obliczeniowa O(xs1+xs2+xs3), pamięciowa O(xs1+xs2+xs3)
  def combine3[A](xs1: List[A], xs2: List[A], xs3: List[A]): List[A] =
    if (xs1 == Nil)
      if (xs2 == Nil)
        if (xs3 == Nil) Nil
        else xs3.head :: combine3(xs1, xs2, xs3.tail)
      else xs2.head :: combine3(xs1, xs2.tail, xs3)
    else xs1.head :: combine3(xs1.tail, xs2, xs3)

  //Złożoność obliczeniowa O(xs1+xs2+xs3), pamięciowa O(1)
  def combine3Tail[A](xs1: List[A], xs2: List[A], xs3: List[A]): List[A] = {
    @tailrec
    def combine3Rec(xs1: List[A], xs2: List[A], xs3: List[A], curr: List[A]): List[A] = (xs1, xs2, xs3) match {
      case (Nil, Nil, Nil) => curr
      case (Nil, Nil, _) => combine3Rec(xs1, xs2, xs3.tail, xs3.head :: curr)
      case (Nil, _, _) => combine3Rec(xs1, xs2.tail, xs3, xs2.head :: curr)
      case _ => combine3Rec(xs1.tail, xs2, xs3, xs1.head :: curr)
    }
    myReverse(combine3Rec(xs1, xs2, xs3, List()))
  }

  //METODY POMOCNICZE
  //Złożoność obliczeniowa O(xs), pamięciowa O(1)
  def myReverse[A](xs: List[A]): List[A] = {
    @tailrec
    def myReverseRec(xs: List[A], res: List[A]): List[A] = {
      if (xs == Nil) res
      else myReverseRec(xs.tail, xs.head :: res)
    }
    myReverseRec(xs, List())
  }

  //Złożoność obliczeniowa O(n) (gdzie n dłuższy ze stringów x, y)
  //Złożoność pamięciowa O(1)
  def myContains(x: String, y: String): Boolean = {
    @tailrec
    def myContainsRec(x1: String, y1: String, x: String, y: String): Boolean = {
      (x1, y1) match {
        case ("", "") => true
        case ("", _) => false
        case (_, "") => true
        case (_, _) => if (x1.head == y1.head) myContainsRec(x1.tail, y1.tail, x, y)
        else myContainsRec(x.tail, y, x.tail, y)
      }
    }
    myContainsRec(x, y, x, y)
  }

}

