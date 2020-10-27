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
