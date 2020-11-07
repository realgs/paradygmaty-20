object L3 extends App{

  //zadanie 1
  def z1 (list: List[Int]): (List[Int], List[Int]) =
    {

    }

  //zadanie 2
  def z2 [A] (list: List[A]): Int =
    {
      if (list == Nil) 0
      else if (list.tail == Nil) 1
      else 1 + z2(list.tail)
    }

  //zadanie 3
  def z3 [A] (list1: List[A], list2: List[A]): List[A] =
  {
    (list1, list2) match
      {
      case (Nil, _) => list2
      case (_ , Nil) => list1
      case (head_1 :: tail_1, head_2 :: tail_2 ) => head_1 :: head_2 :: z3(tail_1, tail_2)
    }

  }
  //zadanie 5

  def z5 [A] (list1: List[A], list2: List[A], list3: List[A]): List[A] = {
    list1 match
    {
      case (Nil) =>
        if (list2 != Nil) z5(list2, list3, list1)
        else if (list3 != Nil) z5(list3, list1, list2)
        else Nil

      case (head :: tail) => head :: z5(tail, list2, list3)
    }
  }


}
