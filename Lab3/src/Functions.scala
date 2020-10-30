import scala.annotation.tailrec

object Functions {

  // Function 1.
  def filter[A](list: List[A], predicate: A => Boolean): List[A] =
    list match {
      case Nil => Nil
      case h :: t => if(predicate(h)) h :: filter(t, predicate) else filter(t, predicate)
    }

  def divide(numbers: List[Int]): (List[Int], List[Int]) =
    (filter(numbers, (x: Int) => x < 0), filter(numbers, (x: Int) => x < 0 && x % 2 != 0))

  // Function 2.
  // time complexity = O(n) where n - list length
  // space complexity = O(1)
  def length[A](list: List[A]): Int = {
    @tailrec
    def lengthIter(list: List[A], acc: Int): Int =
      if(list == Nil) acc
      else lengthIter(list.tail, acc+1)
    lengthIter(list, 0)
  }

  // Function 3.
  // time complexity = O(n) where n - shorter list length
  // space complexity = O(n) where n - shorter list length
  def append[A](list1: List[A], list2: List[A]): List[A] =
    (list1, list2) match {
      case (Nil, _) => list2
      case (_, Nil) => list1
      case (h1 :: t1, h2 :: t2) => h1 :: h2 :: append(t1, t2)
    }

  // Function 4.
  def stringContains(text: String, pattern: String): Boolean = {
    @tailrec
    def stringContainsIter(txt: String, pat: String): Boolean = {
      if(txt.isBlank) false
      else if(pat.isBlank) true
      else if(txt.head == pat.head) {

        if(txt.tail == "" && pat.tail == "") true
        else stringContainsIter(txt.tail, pat.tail)

      } else stringContainsIter(txt.tail, pattern)
    }

    if(pattern == "") true
    else if(text == "") false
    else stringContainsIter(text, pattern)
  }

  def findSingle(elements: List[String], pattern: String): List[String] =
    elements match {
      case Nil => Nil
      case h :: t => if(stringContains(h, pattern)) h :: findSingle(t, pattern) else findSingle(t, pattern)
    }

  def join[A](list1: List[A], list2: List[A]): List[A] =
    list1 match {
      case Nil => list2
      case h :: t => h :: join(t, list2)
    }

  def deleteDuplications(list: List[String]): List[String] =
    list match {
      case Nil => Nil
      case h :: t => h :: deleteDuplications(filter(t, (x: String) => x != h))
    }

  def find(elements: List[String], patterns: List[String]): List[String] = {
    @tailrec
    def findIter(patterns: List[String], acc: List[String]): List[String] =
      if(patterns == Nil) acc
      else findIter(patterns.tail, join(findSingle(elements, patterns.head), acc))
    deleteDuplications(findIter(patterns, Nil))
  }

  // Function 5.
  // time complexity = O(n) where n - sum of first list length and second list length
  // space complexity = O(n) where n - sum of first list length and second list length
  def joinLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    (list1, list2, list3) match {
      case (Nil, Nil, _) => list3
      case (Nil, h :: t, _) => h :: joinLists(Nil, t, list3)
      case (h :: t, _, _) => h :: joinLists(t, list2, list3)
    }

}
