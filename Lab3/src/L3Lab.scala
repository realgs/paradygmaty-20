import scala.annotation.tailrec

object L3Lab extends App {
  // Zadanie 1
  def doubleListSplitBy(list: List[Double], predicate1: Double => Boolean, predicate2: Double => Boolean): (List[Double], List[Double]) = {
    def listFilter(list: List[Double], predicate: Double => Boolean): List[Double] = list match {
      case Nil => Nil
      case head :: tail => if (predicate(head)) head :: listFilter(tail, predicate) else listFilter(tail, predicate)
    }

    (listFilter(list, predicate1), listFilter(list, predicate2))
  }

  // Zadanie 2
  // Zloznosc obliczeniowa - O(n)
  // Zloznosc pamieciowa - O(1) - rekurencja ogonowa
  def listLength[A](list: List[A]): Int = {
    @tailrec
    def listLengthTail(list: List[A], currentLength: Int): Int = {
      if (list == Nil) currentLength
      else listLengthTail(list.tail, currentLength + 1)
    }

    listLengthTail(list, 0)
  }

  // Zadanie 3
  // Zloznosc obliczeniowa - O(2*(n+m)) - musimy na koniec obrocic otrzymana liste, stad 2*
  // Zloznosc pamieciowa - O(1) - rekurencja ogonowa
  def listZipAlternately[A](firstList: List[A], secondList: List[A]): List[A] = {
    @tailrec
    def listZipAlternatelyTail(currentList: List[A], firstTail: List[A], secondTail: List[A]): List[A] = (firstTail, secondTail) match {
      case (Nil, Nil) => listReverse(currentList)
      case (Nil, head :: _) => listZipAlternatelyTail(head :: currentList, Nil, secondTail.tail)
      case (head :: _, Nil) => listZipAlternatelyTail(head :: currentList, firstTail.tail, Nil)
      case (firstHead :: firstTail, secondHead :: secondTail) => listZipAlternatelyTail(secondHead :: firstHead :: currentList, firstTail, secondTail)
    }

    listZipAlternatelyTail(Nil, firstList, secondList)
  }

  // Zadanie 4
  // Zwykła rekurencja
  def stringListSearch(list: List[String], other: String): List[String] = {
    if (list == Nil) Nil
    else if (stringContains(list.head, other)) list.head :: stringListSearch(list.tail, other)
    else stringListSearch(list.tail, other)
  }

  // N fraz - Zwykła rekurencja
  def stringListSearchList(list: List[String], othersList: List[String]): List[String] = {
    if (list == Nil) Nil
    else if (stringContainsList(list.head, othersList)) list.head :: stringListSearchList(list.tail, othersList)
    else stringListSearchList(list.tail, othersList)
  }

  // Rekurencja ogonowa
  def stringListSearchTail(list: List[String], other: String): List[String] = {
    @tailrec
    def stringListSearchTailRec(resultList: List[String], words: List[String], pattern: String): List[String] = {
      if (words == Nil) resultList
      else if (stringContains(words.head, pattern)) stringListSearchTailRec(words.head :: resultList, words.tail, pattern)
      else stringListSearchTailRec(resultList, words.tail, pattern)
    }

    stringListSearchTailRec(Nil, listReverse(list), other)
  }

  // N fraz - Rekurencja ogonowa
  def stringListSearchListTail(list: List[String], othersList: List[String]): List[String] = {
    @tailrec
    def stringListSearchListTailRec(resultList: List[String], words: List[String], patterns: List[String]): List[String] = {
      if (words == Nil) resultList
      else if (stringContainsList(words.head, patterns)) stringListSearchListTailRec(words.head :: resultList, words.tail, patterns)
      else stringListSearchListTailRec(resultList, words.tail, patterns)
    }

    stringListSearchListTailRec(Nil, listReverse(list), othersList)
  }

  // Zadanie 5
  // Zloznosc obliczeniowa - O(n+m) - suma dlugosci dwoch pierwszych list
  // Zloznosc pamieciowa - O(n+m)
  def listMergeWith[A](firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] = (firstList, secondList, thirdList) match {
    case (head :: tail, _, _) => head :: listMergeWith(tail, secondList, thirdList)
    case (Nil, head :: tail, _) => head :: listMergeWith(firstList, tail, thirdList)
    case (_, Nil, _) => thirdList
  }

  // Zloznosc obliczeniowa - O(n+m+p) - suma dlugosci trzech list
  // Zloznosc pamieciowa - O(1)
  def listMergeWithTail[A](firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] = {
    @tailrec
    def listMergeWithTailRec(currentList: List[A], firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] = (firstList, secondList, thirdList) match {
      case (Nil, Nil, Nil) => listReverse(currentList)
      case (head :: tail, _, _) => listMergeWithTailRec(head :: currentList, tail, secondList, thirdList)
      case (Nil, head :: tail, _) => listMergeWithTailRec(head :: currentList, firstList, tail, thirdList)
      case (_, Nil, head :: tail) => listMergeWithTailRec(head :: currentList, firstList, secondList, tail)
    }

    listMergeWithTailRec(Nil, firstList, secondList, thirdList)
  }

  // Funkcje pomocnicze
  private def listReverse[A](list: List[A]): List[A] = {
    @tailrec
    def listReverseTail(list: List[A], currentReversedList: List[A]): List[A] = {
      if (list == Nil) currentReversedList
      else listReverseTail(list.tail, list.head :: currentReversedList)
    }

    listReverseTail(list, Nil)
  }

  private def stringContains(word: String, other: String): Boolean = {
    @tailrec
    def stringContainsTail(currentWord: String, currentOther: String, currentText: String): Boolean = {
      if (currentOther.isEmpty) true
      else if (currentWord.isEmpty) false
      else if (currentWord.head == currentOther.head) stringContainsTail(currentWord.tail, currentOther.tail, currentText)
      else stringContainsTail(currentText.tail, other, currentText.tail)
    }

    stringContainsTail(word, other, word)
  }

  @tailrec
  private def stringContainsList(word: String, otherList: List[String]): Boolean = {
    if (otherList == Nil) false
    else if (stringContains(word, otherList.head)) true
    else stringContainsList(word, otherList.tail)
  }
}
