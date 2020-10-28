package L3
import scala.annotation.tailrec

object L3 {
  // funkcja pomocnicza
  def reverse [A](list: List[A]): List [A] = {
    @tailrec
    def innerReverse [A](list: List[A], revList: List[A]): List[A] =
      list match {
        case Nil => revList
        case head :: tail => innerReverse(tail, head :: revList)
      }
    innerReverse(list, Nil)
  }

  // zadanie 1
  def divide (list: List[Double]): (List[Double], List[Double]) = {
    @tailrec
    def innerDivider (list: List[Double], list1: List[Double], list2: List[Double]): (List[Double], List[Double]) =
      if(list == Nil) (reverse(list1), reverse(list2))
      else if (list.head >= 0) innerDivider(list.tail, list1, list2)
      else if (list.head % 2 == -1) innerDivider(list.tail,  list.head :: list1, list.head :: list2)
      else innerDivider(list.tail, list.head :: list1, list2)
    innerDivider(list, Nil, Nil)
  }

  // zadanie 2: złożoność obliczeniowa: O(n) gdzie n - długość listy
  // złożoność pamięciowa: O(1) (rekursja ogonowa jest optymalizowana)
  def listLength [A](list: List[A]): Int = {
    if (list == Nil) 0
    else {
      @tailrec
      def innerCounter [A](list: List[A], counter: Int): Int =
        if (list.tail != Nil) innerCounter(list.tail, counter + 1)
        else counter
      innerCounter(list, 1)
    }
  }

  // zadanie 3: złożoność obliczeniowa: O(d) a dokładnie wynosi n = d + 1 + (d + k) + 1 gdzie d - długość dłuższej listy, k - długość krótszej listy
  // złożoność pamięciowa: O(1) (rekursja ogonowa jest optymalizowana)
  def mergeLists [A](list1: List[A], list2: List[A]): List[A] = {
    @tailrec
    def innerMergeLists [A](list1: List[A], list2: List[A], connectedList: List[A]): List[A] =
      (list1, list2) match {
        case (Nil, Nil) => reverse (connectedList)
        case (hd1::tl1, hd2::tl2) => innerMergeLists(tl1, tl2, hd2 :: hd1 :: connectedList)
        case (Nil, hd::tl) => innerMergeLists(list1, tl, hd :: connectedList)
        case (hd::tl, Nil) => innerMergeLists(tl, list2, hd :: connectedList)
      }
    innerMergeLists(list1, list2, Nil)
  }

  // metoda pomocnicza do zadania 4
  def contains (String: String, Pattern: String): Boolean = {
    @tailrec
    def innerContains (actualString: String, actualPattern: String, String: String, Pattern: String) : Boolean =
      (actualString, actualPattern) match {
        case ("", "") => true
        case ("", _) => false
        case (_, "") => true
        case (_, _) => if (actualString.head == actualPattern.head) innerContains(actualString.tail, actualPattern.tail, String, Pattern)
        else innerContains(String.tail, Pattern, String.tail, Pattern)
      }
    innerContains(String, Pattern, String, Pattern)
  }

  // zadanie 4 rekursja, wersja z jednym wzorcem
  def find (list: List[String], pattern: String): List[String] = {
    list match {
      case Nil => Nil
      case hd::tl => if(contains(hd, pattern)) hd :: find(tl, pattern)
      else find(tl, pattern)
    }
  }

  // zadanie 4 rekursja ogonowa, wersja z jednym wzorcem
  def findTailRec(list: List[String], pattern: String): List[String] = {
    @tailrec
    def innerFindTailRec (list: List[String], pattern: String, resultList: List[String]): List[String] =
      list match {
        case Nil => reverse(resultList)
        case hd::tl => if (contains(hd, pattern)) innerFindTailRec(tl, pattern, hd :: resultList)
        else innerFindTailRec(tl, pattern, resultList)
      }
    innerFindTailRec(list, pattern, Nil)
  }

  // zadanie 4 rekusrja, wersja z N wzorcami
  def findMany(list: List[String], patternList: List[String]): List[String] = {
    def innerFindMany (list: List[String], currentPatternList: List[String], patternList: List [String]): List[String] =
      (list, currentPatternList) match {
        case (Nil, _) => Nil
        case (_::tl, Nil) => innerFindMany(tl, patternList, patternList)
        case (hd1::tl1, hd2::tl2) => if (contains(hd1, hd2)) hd1 :: innerFindMany(tl1, patternList, patternList)
        else innerFindMany(list, tl2, patternList)
      }
    innerFindMany(list, patternList, patternList)
  }

  // zadanie 4 rekusrja ogonowa, wersja z N wzorcami
  def findManyTailRec(list: List[String], patterns: List[String]): List[String] = {
    @tailrec
    def innerFindManyTailRec (list: List[String], actualPatterns: List[String], patterns: List [String], resultList: List[String]): List[String] =
      (list, actualPatterns) match {
        case (Nil, _) => reverse(resultList)
        case (_::tl, Nil) => innerFindManyTailRec(tl, patterns, patterns, resultList)
        case (hd1::tl1, hd2::tl2) => if (contains(hd1, hd2)) innerFindManyTailRec(tl1, patterns, patterns, hd1 :: resultList)
        else innerFindManyTailRec(list, tl2, patterns, resultList)
      }
    innerFindManyTailRec(list, patterns, patterns, Nil)
  }

  // zadanie 5 rekursja
  def joinLists [A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    (list1, list2, list3) match {
      case (Nil, Nil, Nil) => Nil
      case (hd::tl, _, _) => hd :: joinLists(tl, list2, list3)
      case (Nil, hd::tl, _) => hd :: joinLists(list1, tl, list3)
      case (Nil, Nil, hd::tl) => hd :: joinLists(list1, list2, tl)
    }

  // zadanie 5 rekursja ogonowa
  def joinListsTailRec [A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
    @tailrec
    def innerJoinListsTailRec [A](list1: List[A], list2: List[A], list3: List[A], connectedList: List[A]): List[A] =
      (list1, list2, list3) match {
        case (Nil, Nil, Nil) => reverse (connectedList)
        case (hd::tl, _,  _) => innerJoinListsTailRec(tl, list2, list3, hd :: connectedList)
        case (Nil, hd::tl, _) => innerJoinListsTailRec(list1, tl, list3, hd :: connectedList)
        case (Nil, Nil, hd::tl) => innerJoinListsTailRec(list1, list2, tl, hd :: connectedList)
      }
    innerJoinListsTailRec(list1, list2, list3, Nil)
  }
}


