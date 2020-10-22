def listLength [A](xs: List[A]): Int =
{
  @scala.annotation.tailrec
  def count(xs: List[A], length: Int): Int =
  {
    if(xs == Nil) length
    else count(xs.tail, length + 1)
  }
  count(xs, 0)
}

// Złożoność obliczeniowa: liniowa względem długości listy
// Złożoność pamięciowa: stała

listLength(List(5, 4, 3, 2)) == 4
listLength(List()) == 0
listLength(List(List(1, 2, 3), List(1, 2, 3, 4))) == 2
listLength(List("1", "2")) == 2
