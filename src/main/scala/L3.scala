import com.sun.org.apache.bcel.internal.generic.GOTO

import scala.annotation.tailrec

object L3 extends App {
  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def reverseHelper(list: List[A], reversedList: List[A]): List[A] = {
      if (list == Nil) reversedList
      else reverseHelper(list.tail, list.head :: reversedList)
    }

    reverseHelper(list, Nil)
  }

  // zadanie 1
  def divideList(list: List[Int]): (List[Int], List[Int]) = {
    @tailrec
    def divideListHelper(numbers: List[Int], negativeNumbers: List[Int], negativeOddNumbers: List[Int]): (List[Int], List[Int]) = {
      if (numbers == Nil) (negativeNumbers, negativeOddNumbers)
      else if (numbers.head < 0) {
        if (numbers.head % 2 != 0) divideListHelper(numbers.tail, numbers.head :: negativeNumbers, numbers.head :: negativeOddNumbers)
        else divideListHelper(numbers.tail, numbers.head :: negativeNumbers, negativeOddNumbers)
      }
      else divideListHelper(numbers.tail, negativeNumbers, negativeOddNumbers)
    }

    divideListHelper(reverse(list), Nil, Nil)
  }

  // zadanie 2
  // Zlozonosc obliczeniowa: O(n)
  // Zlozonosc pamieciowa: O(1)
  def length[A](list: List[A]): Int = {
    if (list == Nil) 0
    else if (list.tail == Nil) 1
    else 1 + length(list.tail)
  }

  // zadanie 3
  // Zlozonosc obliczeniowa: O(n+m), gdzie n to wielkosc pierwszej listy a m to wielkosc drugiej
  // Zlozonosc pamieciowa: O(n+m), gdzie n to rozmiar pierwszej listy a m to rozmiar drugiej
  def concatenateLists[A](firstList: List[A], secondList: List[A]): List[A] = {
    @tailrec
    def concatenateListsHelper(resultList: List[A], firstList: List[A], secondList: List[A], takeFromFirstList: Boolean): List[A] = {
      if (firstList == Nil && secondList == Nil) {
        reverse(resultList)
      }
      else if (firstList == Nil) {
        concatenateListsHelper(secondList.head :: resultList, Nil, secondList.tail, true)
      }
      else if (secondList == Nil) {
        concatenateListsHelper(firstList.head :: resultList, firstList.tail, Nil, false)
      }
      else if (takeFromFirstList) {
        concatenateListsHelper(firstList.head :: resultList, firstList.tail, secondList, false)
      }
      else {
        concatenateListsHelper(secondList.head :: resultList, firstList, secondList.tail, true)
      }
    }

    concatenateListsHelper(Nil, firstList, secondList, true)
  }

  // zadanie 4 - funkcje pomocnicze
  // Zlozonosc obliczeniowa: O(n), gdzie n to wielkosc wyrazu w ktorym szukamy wzorca
  // Zlozonosc pamieciowa: O(1)
  def contains(word: String, pattern: String): Boolean = {
    @tailrec
    def containsHelper(actualWord: String, actualPattern: String): Boolean = {
      if (actualWord == "") false
      else if (actualWord.head == actualPattern.head) {
        if (actualPattern.tail == "") true
        else containsHelper(actualWord.tail, actualPattern.tail)
      }
      else containsHelper(actualWord.tail, pattern)
    }

    if (pattern == "") true
    else if (length(pattern.toList) > length(word.toList)) false
    else containsHelper(word, pattern)
  }

  // Zlozonosc obliczeniowa: O(n*m), gdzie n to wielkosc wyrazu w ktorym szukamy wzorca, a m to wielkosc listy fraz
  // Zlozonosc pamieciowa: O(1)
  @tailrec
  def containsMultiplePhrases(word: String, phrases: List[String]): Boolean = {
    if (phrases == Nil) false
    else if (contains(word, phrases.head)) true
    else containsMultiplePhrases(word,phrases.tail)
  }

  // zadanie 4 bez N fraz (zwykła rekurencja)
  def find(words: List[String], pattern: String): List[String] = {
    if (words == Nil) Nil
    else if (contains(words.head, pattern)) words.head :: find(words.tail, pattern)
    else find(words.tail, pattern)
  }

  // zadanie 4 z N frazami (zwykła rekurencja)
  // Zlozonosc obliczeniowa: O(n*m*l), (n*m) to złożonosc funkcji containsMultiplePhrases, a l to wielkosc listy slow
  // Zlozonosc pamieciowa: O(1)
  def findMultiplePhrases(words: List[String], patterns: List[String]) : List[String] = {
    if(words == Nil) Nil
    else if(containsMultiplePhrases(words.head, patterns)) words.head :: findMultiplePhrases(words.tail, patterns)
    else findMultiplePhrases(words.tail, patterns)
  }

  // zadanie 4 bez N fraz (rekurencja ogonowa)
  // Zlozonosc obliczeniowa: O(n*m*l), (n*m) to złożonosc funkcji containsMultiplePhrases, a l to wielkosc listy slow
  // Zlozonosc pamieciowa: O(n), n to ilosc slow ktore zgadzaja sie z wzorcem
  def findTail(words: List[String], pattern: String): List[String] = {
    @tailrec
    def findHelper(resultList: List[String], words: List[String], pattern: String): List[String] = {
      if (words == Nil) resultList
      else if (contains(words.head, pattern)) findHelper(words.head :: resultList, words.tail, pattern)
      else findHelper(resultList, words.tail, pattern)
    }

    findHelper(Nil, reverse(words), pattern)
  }

  // zadanie 4 z N frazami (rekurencja ogonowa)
  def findMultiplePhrasesTail(words: List[String], patterns: List[String]) : List[String] = {
    @tailrec
    def findHelper(resultList: List[String], words: List[String], patterns: List[String]): List[String] = {
      if (words == Nil) resultList
      else if (containsMultiplePhrases(words.head, patterns)) findHelper(words.head :: resultList, words.tail, patterns)
      else findHelper(resultList, words.tail, patterns)
    }

    findHelper(Nil, reverse(words), patterns)
  }

  // zadanie 5 (zwykła rekurencja)
  // Zlozonosc obliczeniowa: O(n+m), gdzie n to wielkosc pierwszej listy a m to wielkosc drugiej
  // Zlozonosc pamieciowa: O(1)
  def join[A](firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] = {
    if (secondList == Nil) thirdList
    else if (firstList == Nil) secondList.head :: join(firstList, secondList.tail, thirdList)
    else firstList.head :: join(firstList.tail, secondList, thirdList)
  }

  // zadanie 5 (rekurencja ogonowa)
  // Zlozonosc obliczeniowa: O(n+m+k), gdzie n to wielkosc pierwszej listy, m to wielkosc drugiej, a k trzeciej
  // Zlozonosc pamieciowa: O(n+m+k)
  def joinTail[A](firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] = {
    @tailrec
    def joinTailHelper(resultList: List[A], firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] = {
      if (firstList == Nil && secondList == Nil && thirdList == Nil) {
        reverse(resultList)
      }
      else if (secondList == Nil) {
        joinTailHelper(thirdList.head :: resultList, firstList, secondList, thirdList.tail)
      }
      else if (firstList == Nil) {
        joinTailHelper(secondList.head :: resultList, firstList, secondList.tail, thirdList)
      }
      else {
        joinTailHelper(firstList.head :: resultList, firstList.tail, secondList, thirdList)
      }
    }

    joinTailHelper(Nil, firstList, secondList, thirdList)
  }
}
