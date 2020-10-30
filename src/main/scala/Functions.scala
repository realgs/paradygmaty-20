import scala.annotation.tailrec

object Functions {
  def split(list: List[Int]): (List[Int], List[Int]) = {
    @tailrec
    def splitInner(list: List[Int], first: List[Int], second: List[Int]): (List[Int], List[Int]) =
      list match {
        case Nil => (reverse(first), reverse(second))
        case h :: t if h < 0 => splitInner(t, h :: first, if (h % 2 != 0) h :: second else second)
        case _ :: t => splitInner(t, first, second)
      }

    if(list == null) (Nil, Nil) else splitInner(list, List(), List())
  }

  // O(n) = n
  def length[A](list: List[A]): Int = {
    @tailrec
    def lengthInner(list: List[A], accum: Int): Int =
      if (list == Nil) accum else lengthInner(list.tail, accum + 1)

    if (list == null) 0 else lengthInner(list, 0)
  }


  // O(n) = length of the smaller list
  def merge[A](first: List[A], second: List[A]): List[A] = {
    (first, second) match {
      case (Nil, _) => second
      case (_, Nil) => first
      case (fH :: fT, sH :: sT) => fH :: sH :: merge(fT, sT)
    }
  }

  def find(list: List[String], element: String) = {
    @tailrec
    def find(list: List[String], accumList: List[String]): List[String] =
      if(list == Nil) accumList else find(list.tail, if(isSubString(element, list.head)) list.head :: accumList else accumList)
    find(list, List())
  }

  // Utility functions below

  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def reverseInner(list: List[A], revList: List[A]): List[A] =
      if (list == Nil) revList else reverseInner(list.tail, list.head :: revList)

    reverseInner(list, List())
  }

  @tailrec
  def isSubString(string: String, text: String): Boolean =
    if(text == "") false else if(string == text) true else isSubString(string, text.substring(1))

  def append[A](left: List[A], right: List[A]): List[A] =
    (left, right) match {
      case (Nil, )
    }



}
