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
  (reverse(first), reverse(second))
}

splitNegative(List(-3, -6, 8, -9, 13)) == (List(-3, -6, -9), List(-3, -9))
splitNegative(List(-1, -2, -3, -4, -5, -6)) == (List(-1, -2, -3, -4, -5, -6), List(-1, -3, -5))
splitNegative(List(1, 2, 3, 4)) == (List(), List())
