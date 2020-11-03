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

    if (list == null) (Nil, Nil) else splitInner(list, List(), List())
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
    if(first == Nil) second
    else if(second == Nil) first
    else first.head :: second.head :: merge(first.tail, second.tail)
  }

  def find(list: List[String], keywords: List[String]): List[String] = {
    if(list == Nil) Nil
    else if(containsList(list.head, tokens = keywords)) list.head :: find(list.tail, keywords)
    else find(list.tail, keywords)
  }

  def findTail(list: List[String], keywords: List[String]): List[String] = {

    @tailrec
    def findInner(list: List[String], keywords: List[String], accum: List[String]): List[String] =
      if(list == Nil) reverse(accum)
      else if(containsList(list.head, tokens = keywords)) findInner(list.tail, keywords, list.head :: accum)
      else findInner(list.tail, keywords, accum)

    findInner(list, keywords, List())
  }

  // O(n) = first.length + second.length
  def join[A](first: List[A], second: List[A], third: List[A]): List[A] =
    (first, second) match {
      case (Nil, Nil) => third
      case (Nil, h :: t) => h :: join(first, t, third)
      case (h :: t, _) => h :: join(t, second, third)
    }

  // O(n) = sum of length of 3 lists
  def joinTail[A](first: List[A], second: List[A], third: List[A]): List[A] = {
    @tailrec
    def joinInner(first: List[A], second: List[A], third: List[A], accum: List[A]): List[A] =
      (first, second, third) match {
        case (Nil, Nil, Nil) => reverse(accum)
        case (h :: t, _, _) => joinInner(t, second, third, h :: accum)
        case (Nil, h :: t, _) => joinInner(first, t, third, h :: accum)
        case (Nil, Nil, h :: t) => joinInner(first, second, t, h :: accum)
      }
    joinInner(first, second, third, List())
  }


  // Utility functions below

  // O(n) = n
  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def reverseInner(list: List[A], revList: List[A]): List[A] =
      if (list == Nil) revList else reverseInner(list.tail, list.head :: revList)

    reverseInner(list, List())
  }

  @tailrec
  def containsList(string: String, tokens: List[String]): Boolean =
    if (tokens == Nil) false
    else if (contains(string, tokens.head)) true
    else containsList(string, tokens.tail)

  def contains(string: String, token: String): Boolean = {
    @tailrec
    def containsInner(currStr: List[Char], currToken: List[Char]): Boolean =
      (currStr, currToken) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (sH :: sT, tH :: tT) => if (sH == tH) containsInner(sT, tT)
        else containsInner(sT, token.toList)
      }

    containsInner(string.toList, token.toList)
  }

}
