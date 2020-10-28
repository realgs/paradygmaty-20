//zad 3 - time complex O(n), where n = list1.length, space complex O(n) - n times on the stack
def concatList[A] (l1 : List[A], l2 : List[A]) : List[A] =
  (l1,l2) match {
    case (Nil, _) => l2
    case (_, Nil) => l1
    case (h1 :: t1, h2 :: t2) => h1 :: h2 :: concatList(l1.tail, l2.tail)

  }
concatList(List(1,2,3), List(5,6,7))
concatList(List(), List(5,6,7))
concatList(List(1,2,3), List())
concatList(List(), List())