package lista3

object Zadanie_3 {
  // złożonosćią obliczeniową jest długośc mniejszej z dwoch list
  // O(1) - złożonośc pamięciowa

  def connect[A](list1: List[A], list2: List[A]): List[A] =
    (list1, list2) match {
      case (_, Nil) => list1
      case (Nil, _) => list2
      case (_, _) => list1.head :: list2.head :: connect(list1.tail, list2.tail)
    }
}
