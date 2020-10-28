

object l3 {
  //Lista3

  //Zadanie1
  def listSplit(xs: List[Int]): (List[Int], List[Int]) =
    xs match {
      case Nil => (Nil, Nil)
      case h :: tl => {
        var (left, right) = listSplit(tl)
        if (h < 0 && h % 2 == (-1)) (h :: left, h :: right)
        else if (h < 0) (h :: left, right)
        else (left, right)
      }
    }

  //Zadanie2
  def listLength[A](xs: List[A]): Int =
    if (xs.nonEmpty) 1 + listLength(xs.tail) else 0

  //Zadanie3
  def connect[A](xs: List[A], ys: List[A]): List[A] =
    (xs, ys) match {
      case (Nil, Nil) => Nil
      case ( _ , Nil) => xs
      case (Nil, _ ) => ys
      case ( _ , _ ) => xs.head :: ys.head :: connect(xs.tail, ys.tail)
    }

  //Zadanie5
  def merge[A](xs: List[A], ys: List[A]): List[A] =
    (xs, ys) match {
      case (xs, Nil) => xs
      case (Nil, ys) => ys
      case (hx :: tx, ys) => hx :: merge(tx, ys)
    }

  def joinLists[A](xs: List[A], ys: List[A], zs: List[A]): List[A] =
    merge(xs, merge(ys, zs))
}

