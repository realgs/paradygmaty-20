package List3

import scala.annotation.tailrec

object L3 {

  //Własne implementacje funkcji pomocniczych
  def contains(word1: String, word2: String): Boolean = {
    @tailrec
    def containsIter(word1: String, word2: String, checkString1: String, checkString2: String): Boolean =
      (word1, word2) match {
        case ("", "") => true
        case ("", _) => false
        case (_, "") => true
        case (_, _) if word1 == word2 => true
        case (_, _) => if (word1.head == word2.head) containsIter(word1.tail, word2.tail, checkString1, checkString2)
                      else containsIter(checkString1.tail, checkString2, checkString1.tail, checkString2)
      }
    containsIter(word1, word2, word1, word2)
  }

  def reverse[T](list: List[T]): List[T] = {
    @tailrec
    def reverseTail(list: List[T], resultList: List[T]): List[T] =
      if (list == Nil) resultList
      else reverseTail(list.tail, list.head :: resultList)
    reverseTail(list, Nil)
  }

  //Zadanie 1
  def negativeLists(list: List[Double]): (List[Double], List[Double]) = {
    @tailrec
    def negativeListsIter(list: List[Double], list1: List[Double], list2: List[Double]): (List[Double], List[Double]) = {
      if (list == Nil) (list1, list2)
      else if (list.head >= 0) negativeListsIter(list.tail, list1, list2)
      else if (list.head % 2 == -1) negativeListsIter(list.tail, list.head :: list1, list.head :: list2)
      else negativeListsIter(list.tail, list.head :: list1, list2)
    }
    val (list1, list2) = negativeListsIter(list, List(), List())
    (reverse(list1), reverse(list2))
  }

  //Zadanie 2
  def countLength[T](list: List[T]): Int = {
    @tailrec
    def countLengthIter(list: List[T], acc: Int): Int =
      if (list == Nil) acc
      else countLengthIter(list.tail, acc+1)
    countLengthIter(list, 0)
  }

  //Zadanie 3
  def mergeLists[T](list1: List[T], list2: List[T]): List[T] = {
    if (list1 == Nil) if (list2 != Nil) list2 else List()
    else if (list2 == Nil) list1 else list1.head :: mergeLists(list2, list1.tail)
  }

  //Zadanie 4 z rekurencją nieogonową
  def findElem(list: List[String], element: String): List[String] = {
    if (list == Nil) Nil
    else if (contains(list.head, element)) list.head :: findElem(list.tail, element) else findElem(list.tail, element)
  }

  //Zadanie 4 z rekurencją ogonową
  def findElemTail(list: List[String], element: String): List[String] = {
    @tailrec
    def findElemIter(list: List[String], resultList: List[String]): List[String] =
      if (list == Nil) resultList
      else if (contains(list.head, element)) findElemIter(list.tail, list.head :: resultList)
      else findElemIter(list.tail, resultList)
    reverse(findElemIter(list, Nil))
  }

  //Zadanie 4 z N frazami z rekurencją nieogonową
  def findMoreElem(list: List[String], elements: List[String]): List[String] = {
    def findMoreElemIter(list: List[String], elements: List[String], cloneList: List[String]): List[String] = {
      (list, elements) match {
        case (Nil, _) => Nil
        case (_, Nil) => if (cloneList == Nil) Nil else findMoreElemIter(list.tail, cloneList, cloneList)
        case (h1 :: t1, h2 :: t2) => if (contains(h1, h2)) h1 :: findMoreElemIter(t1, cloneList, cloneList)
        else findMoreElemIter(list, t2, cloneList)
      }
    }
    findMoreElemIter(list, elements, elements)
  }

  //Zadanie 4 z N frazami z rekurencją ogonową
  def findMoreElemTail(list: List[String], elements: List[String]): List[String] = {
    @tailrec
    def findMoreElemIter(list: List[String], elements: List[String], cloneList: List[String], resultList: List[String]): List[String] = {
      (list, elements) match {
        case (Nil, _) => resultList
        case (_, Nil) => if (cloneList == Nil) resultList else findMoreElemIter(list.tail, cloneList, cloneList, resultList)
        case (h1 :: t1, h2 :: t2) => if (contains(h1, h2)) findMoreElemIter(t1, cloneList, cloneList, h1 :: resultList)
        else findMoreElemIter(list, t2, cloneList, resultList)
      }
    }
    reverse(findMoreElemIter(list, elements, elements, Nil))
  }

  //Zadanie 5 z rekurencją nieogonową
  def joinLists[T](list1: List[T], list2: List[T], list3: List[T]): List[T] = {
      if (list1 != Nil) list1.head :: joinLists(list1.tail, list2, list3)
      else if (list2 != Nil) list2.head :: joinLists(list1, list2.tail, list3)
      else if (list3 != Nil) list3.head :: joinLists(list1, list2, list3.tail)
      else Nil
  }

  //Zadanie 5 z rekurencją ogonową
  def joinListsTail[T](list1: List[T], list2: List[T], list3: List[T]): List[T] = {
    @tailrec
    def joinListsIter(list1: List[T], list2: List[T], list3: List[T], resultList: List[T]): List[T] = {
      if (list1 != Nil) joinListsIter(list1.tail, list2, list3, list1.head :: resultList)
      else if (list2 != Nil) joinListsIter(list1, list2.tail, list3, list2.head :: resultList)
      else if (list3 != Nil) joinListsIter(list1, list2, list3.tail, list3.head :: resultList)
      else resultList
    }
    reverse(joinListsIter(list1, list2, list3, Nil))
  }
}