def connect [A](first: List[A], second: List[A]): List[A] =
{
  @scala.annotation.tailrec
  def connectIter(connected: List[A], first: List[A], second: List[A]): List[A] =
  {
    (connected, first, second) match
    {
      case (connected, Nil, Nil) => connected
      case (connected, first, Nil) => connected.reverse ::: first
      case (connected, Nil, second) => connected.reverse ::: second
      case (connected, first, second) =>
        connectIter(second.head :: first.head :: connected, first.tail, second.tail)
    }
  }
  connectIter(List(), first, second)
}

// Złożoność obliczeniowa pesymistyczna: O(n + m) gdzie n to długość pierwszej listy, a m to długość drugiej listy
// Złożoność pamięciowa: stała

connect(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)
connect(List(1, 3, 5, 7), List(2, 4, 6, 8, 9, 10, 11)) == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
connect(List(), List(1, 2, 3)) == List(1, 2, 3)
connect(List(1, 2, 3), List()) == List(1, 2, 3)
