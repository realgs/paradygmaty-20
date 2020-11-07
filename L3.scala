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



}
