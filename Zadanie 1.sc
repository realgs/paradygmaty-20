def splitNegative (list: List[Int]): (List[Int], List[Int]) =
{
  def split(original: List[Int], first: List[Int], second: List[Int]): (List[Int], List[Int]) =
  {
    if(original == Nil) (first, second)
    else if (original.head < 0)
    {
      if(original.head % 2 != 0)
        split(original.tail, original.head :: first, original.head :: second)
      else
        split(original.tail, original.head :: first, second)
    }
    else
      split(original.tail, first, second)
  }
  val (first, second) = split(list, List(), List())
  (first.reverse, second.reverse)
}

splitNegative(List(-3, -6, 8, -9, 13)) == (List(-3, -6, -9), List(-3, -9))
splitNegative(List(-1, -2, -3, -4, -5, -6)) == (List(-1, -2, -3, -4, -5, -6), List(-1, -3, -5))
splitNegative(List(1, 2, 3, 4)) == (List(), List())
