def reverse[A](list: List[A]): List[A] =
{
  @scala.annotation.tailrec
  def reverseIter(list: List[A], reverse: List[A]): List[A] =
  {
    if(list == Nil) reverse
    else reverseIter(list.tail, list.head :: reverse)
  }
  reverseIter(list, List())
}

reverse(List(1, 2, 3, 4)) == List(4, 3, 2, 1)
reverse(List()) == List()

def joinLists[A](first: List[A], second: List[A], third: List[A]): List[A] =
{
  (first, second, third) match
  {
    case (h :: t, _, _) => h :: joinLists(t, second, third)
    case (Nil, h :: t, _) => h :: joinLists(Nil, t, third)
    case (Nil, Nil, third) => third
  }
}

// Złożoność obliczeniowa i pamięciowa: liniowa względem sumy długości pierwszej i drugiej listy

joinLists(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9)
joinLists(List(), List(1), List(1, 2, 3)) == List(1, 1, 2, 3)
joinLists(List(1, 2, 3), List(), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6)
joinLists(List(), List(), List(1, 2, 3)) == List(1, 2, 3)

def joinListsTail[A](first: List[A], second: List[A], third: List[A]): List[A] =
{
  @scala.annotation.tailrec
  def joinListsIter(first: List[A], second: List[A], third: List[A], result: List[A])
  : List[A] =
  {
    (first, second, third, result) match
    {
      case (h :: t, _, _, result) => joinListsIter(t, second, third, h :: result)
      case (Nil, h :: t, _, result) => joinListsIter(Nil, t, third, h :: result)
      case (Nil, Nil, h :: t, result) => joinListsIter(Nil, Nil, t, h :: result)
      case (Nil, Nil, Nil, result) => result.reverse
    }
  }
  joinListsIter(first, second, third, List())
}

// Złożoność obliczeniowa: liniowa względem długości wszystkich trzech list
// Złożoność pamięciowa: stała

joinListsTail(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9)
joinListsTail(List(), List(1), List(1, 2, 3)) == List(1, 1, 2, 3)
joinListsTail(List(1, 2, 3), List(), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6)
joinListsTail(List(), List(), List(1, 2, 3)) == List(1, 2, 3)
