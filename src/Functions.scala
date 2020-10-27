import Utils.{contains, doesFit, filter, rev}

import scala.annotation.tailrec

object Functions {
  // Task 1
  def splitElements(list: List[Int]): (List[Int], List[Int]) =
    (filter(list, (n: Int) => n < 0), filter(list, (n: Int) => n < 0 && n % 2 != 0))

  // Task 2 - time complex O(n) where n = list.length, space complex O(1)
  def listLength[A] (list: List[A]): Int = {
    @tailrec
    def listLengthHelper(list: List[A], length: Int): Int =
      if(list == Nil) length
      else listLengthHelper(list.tail, length + 1)

    listLengthHelper(list, 0)
  }

  // Task 3 time complex O(n), where n = list1.length, space complex O(n) - n times on the stack
  def concatLists[A](list1: List[A], list2: List[A]): List[A] =
    list1 match {
      case head :: tail => head :: concatLists(list2, tail)
      case _ => list2
    }

  // Task 4
  // single phrase tailrec
  def findElement(values: List[String], element: String): List[String] = {
    @tailrec
    def findHelper(resultList: List[String], values: List[String], element: String): List[String] = {
      if (values == Nil) rev(resultList) // rev(resultList) if the in-out order is nescessary
      else findHelper(if(contains(values.head, element)) values.head :: resultList else resultList, values.tail, element)
    }
    findHelper(List(), values, element)
  }

  // single phrase rec
  def findEl(list: List[String], element: String): List[String] =
    list match {
      case Nil => Nil
      case head :: tail => if(contains(head, element)) head :: findEl(tail, element) else findEl(tail, element)
    }

  // N phrases tailrec
  def findElements(values: List[String], query: List[String]): List[String] = {
    @tailrec
    def findHelper(resultList: List[String], values: List[String], query: List[String]): List[String] = {
      if (values == Nil) rev(resultList) // rev(resultList) if the in-out order is nescessary
      else findHelper(if(doesFit(values.head, query)) values.head :: resultList else resultList, values.tail, query)
    }
    findHelper(List(), values, query)
  }

  // N phrases rec
  def findEls(values: List[String], query: List[String]): List[String] =
    values match {
      case Nil => Nil
      case head :: tail => if(doesFit(head, query)) head :: findEls(tail, query) else findEls(tail, query)
    }

  // Task 5 - time complex O(n), where n = x.length + y.length + 1 (z), size complex O(n)
  def joinLists[A](x: List[A], y: List[A], z: List[A]): List[A] =
    (x, y, z) match {
      case (head :: tail, _, _) => head :: joinLists(tail, y, z)
      case (Nil, head :: tail, _) => head :: joinLists(Nil, tail, z)
      case (Nil, Nil, z) => z
    }

  // time complex O(n), where n = x.length + y.length + z.length, size complex O(1)
  def joinListsTail[A](x: List[A], y: List[A], z: List[A]): List[A] = {
    @tailrec
    def joinListsHelper(finalList: List[A], x: List[A], y: List[A], z: List[A]): List[A] =
      (finalList, x, y, z) match {
        case (finalList, head :: tail, _, _) => joinListsHelper(head :: finalList, tail, y, z)
        case (finalList, Nil, head :: tail, _) => joinListsHelper(head :: finalList, Nil, tail, z)
        case (finalList, Nil, Nil, head :: tail) => joinListsHelper(head :: finalList, Nil, Nil, tail)
        case (finalList, Nil, Nil, Nil) => rev(finalList)
      }
    joinListsHelper(List(), x, y, z)
  }

}
