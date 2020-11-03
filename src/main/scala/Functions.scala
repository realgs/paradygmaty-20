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
    (first, second) match {
      case (Nil, _) => second
      case (_, Nil) => first
      case (fH :: fT, sH :: sT) => fH :: sH :: merge(fT, sT)
    }
  }

  def find(list: List[String], keywords: List[String]): List[String] = {
    (list, keywords) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (h :: t, _) => if (containsList(h, tokens = keywords)) h :: find(t, keywords)
      else find(t, keywords)
    }
  }

  def findTail(list: List[String], keywords: List[String]): List[String] = {

    @tailrec
    def findInner(list: List[String], keywords: List[String], accum: List[String]): List[String] =
      (list, keywords) match {
        case (Nil, _) => reverse(accum)
        case (_, Nil) => reverse(accum)
        case (h :: t, _) => if (containsList(h, tokens = keywords)) findInner(t, keywords, h :: accum)
        else findInner(t, keywords, accum)
      }

    findInner(list, keywords, List())
  }

  def join[A](first: List[A], second: List[A], third: List[A]): List[A] =
    (first, second) match {
      case (Nil, Nil) => third
      case (Nil, h :: t) => h :: join(first, t, third)
      case (h :: t, _) => h :: join(t, second, third)
    }

  def joinTail[A](first: List[A], second: List[A], third: List[A]): List[A] = {
    @tailrec
    def joinInner(first: List[A], second: List[A], accum: List[A]): List[A] =
      (first, second) match {
        case (Nil, Nil) => reverse(accum)
        case (Nil, h :: t) => joinInner(first, t, h :: accum)
        case (h :: t, _) => joinInner(t, second, h :: accum)
      }
    joinInner(joinInner(first, second, List()), third, List())
  }


  // Utility functions below

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
