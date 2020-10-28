object l3_Task4 {

  //Zadanie4
  def my_contains(y: String, x: String): Boolean = {
    def containsIter(ys: List[Char], xs: List[Char]): Boolean = {
      (xs, ys) match {
        case (Nil, _) => true
        case (_, Nil) => false
        case (xs, ys) => if (ys.head == xs.head) containsIter(ys.tail, xs.tail)
        else containsIter(ys.tail, x.toList)
      }
    }
    containsIter(y.toList, x.toList)
  }

  def findCheck[A](xs: List[A], y: A): Boolean = {
    if (xs != Nil) {
      if (my_contains(y.toString, xs.head.toString)) true else findCheck(xs.tail, y)
    } else false
  }

  //rekursja nieogonowa
  def find[A](ys: List[A], xs: List[A]): List[A] =
    if (ys != Nil) {
      if (findCheck(xs, ys.head)) ys.head :: find(ys.tail, xs)
      else find(ys.tail, xs)
    } else Nil

  def my_append[A](xs: List[A], y: A): List[A] ={
    if(xs!=Nil) xs.head::my_append(xs.tail, y)
    else y::Nil
  }

  //rekursja ogonowa
  def findTailrec[A](ys: List[A], xs: List[A]): List[A] = {
    def findIter(ys: List[A], xs: List[A], result: List[A]): List[A] =
      if (ys != Nil) findIter(ys.tail, xs, if (findCheck(xs, ys.head)) my_append(result, ys.head) else result) else result

    findIter(ys, xs, List())
  }
}

