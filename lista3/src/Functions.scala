import scala.annotation.tailrec

object Functions {
  //zadanie 1
  def split(list: List[Int]): (List[Int], List[Int]) = {
    @tailrec
    def splitIter(list: List[Int], negative: List[Int], negativeAndOdd: List[Int]): (List[Int], List[Int]) = {
      if (list == Nil) (reverse(negative), reverse(negativeAndOdd))
      else if (list.head < 0) {
        if (list.head % 2 == -1) splitIter(list.tail, list.head :: negative, list.head :: negativeAndOdd)
        else splitIter(list.tail, list.head :: negative, negativeAndOdd)
      }
      else splitIter(list.tail, negative, negativeAndOdd)
    }
    splitIter(list, Nil, Nil)
  }

  //zadanie 2
  def length[A](list: List[A]): Int = {
    @tailrec
    def lengthIter(list: List[A], length: Int): Int = {
      if (list == Nil) length
      else lengthIter(list.tail, length + 1)
    }
    lengthIter(list, 0)
  }

  //zadanie 3
  def merge[A](firstList: List[A], secondList: List[A]): List[A] = {
    @tailrec
    def mergeIter(firstList: List[A], secondList: List[A], returnList: List[A]): List[A] = {
      (firstList, secondList) match {
        case (Nil, Nil) => reverse(returnList)
        case (head :: tail, Nil) => mergeIter(tail, secondList, head :: returnList)
        case (Nil, head :: tail) => mergeIter(firstList, tail, head :: returnList)
        case (head :: tail, _) => mergeIter(secondList, tail, head :: returnList)
      }
    }
    mergeIter(firstList, secondList, Nil)
  }

  //zadanie 4
  @tailrec
  private def matchStringToElementHelper(argument: String, element: String): Boolean = {
    (argument, element) match {
      case ("", "") => return true
      case ("", _) => return false
      case (_, "") => return true
      case (_, _) =>
        if (argument.head == element.head) matchStringToElementHelper(argument.tail, element.tail)
        else matchStringToElementHelper(argument.tail, element)
    }
  }

  private def matchStringToElement(argument: String, element: String): Boolean = {
    if (stringLength(argument) == 0 && stringLength(element) == 0) return true
    else if (argument.length < element.length) return false;
    else matchStringToElementHelper(argument, element)
  }

  def find_Tail(list: List[String], element: String): List[String] = {
    @tailrec
    def find_TailIter(list: List[String], element: String, returnList: List[String]): List[String] = {
      if (list == Nil) reverse(returnList)
      else if (matchStringToElement(list.head, element)) find_TailIter(list.tail, element, list.head :: returnList)
      else find_TailIter(list.tail, element, returnList)
    }
    find_TailIter(list, element, Nil)
  }

  def findEach_Tail(list: List[String], elements: List[String]): List[String] = {
    @tailrec
    def findEach_Tail_Iter(list: List[String], elements: List[String], returnList: List[String]): List[String] = {
      if (elements == Nil) returnList
      else {
        @tailrec
        def addMatchesToReturnList(matches: List[String], returnList: List[String]): List[String] = {
          if (matches == Nil) returnList
          else if (!contains(returnList, matches.head)) addMatchesToReturnList(matches.tail, matches.head :: returnList)
          else addMatchesToReturnList(matches.tail, returnList)
        }
        findEach_Tail_Iter(list, elements.tail, addMatchesToReturnList(find_Tail(list, elements.head), returnList))
      }
    }
    reverse(findEach_Tail_Iter(list, elements, Nil))
  }

  def find(list: List[String], element: String): List[String] = {
    if (list == Nil) Nil
    else if (matchStringToElement(list.head, element)) list.head :: find(list.tail, element)
    else find(list.tail, element)
  }

  def findEach(list: List[String], elements: List[String]): List[String] = {
    def findEach_Iter(list: List[String], elements: List[String]): List[String] = {
      if (elements == Nil) Nil
      else {
        def createListWithRepetitions(matches: List[String]): List[String] = {
          if (matches == Nil) Nil
          else matches.head :: createListWithRepetitions(matches.tail)
        }
        createListWithRepetitions(find(list, elements.head)) ::: findEach(list, elements.tail)
      }
    }
    newListWithoutRepetition(findEach_Iter(list, elements), Nil)
  }

  @tailrec
  private def newListWithoutRepetition(list: List[String], returnList: List[String]): List[String] = {
    if (list == Nil) reverse(returnList)
    else if (!contains(returnList, list.head)) newListWithoutRepetition(list.tail, list.head :: returnList)
    else newListWithoutRepetition(list.tail, returnList)
  }

  //zadanie 5
  def joinLists_TailRec[A](firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] = {
    @tailrec
    def joinLists_TailRecIter(firstList: List[A], secondList: List[A], thirdList: List[A], returnList: List[A]): List[A] = {
      (firstList, secondList, thirdList) match {
        case (head :: tail, _, _) => joinLists_TailRecIter(tail, secondList, thirdList, head :: returnList)
        case (Nil, head :: tail, _) => joinLists_TailRecIter(firstList, tail, thirdList, head :: returnList)
        case (Nil, Nil, head :: tail) => joinLists_TailRecIter(firstList, secondList, tail, head :: returnList)
        case (Nil, Nil, Nil) => reverse(returnList)
      }
    }
    joinLists_TailRecIter(firstList, secondList, thirdList, Nil)
  }

  def joinLists[A](firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] = {
    (firstList, secondList, thirdList) match {
      case (head :: tail, _, _) => head :: joinLists(tail, secondList, thirdList)
      case (Nil, head :: tail, _) => head :: joinLists(firstList, tail, thirdList)
      case (Nil, Nil, _) => thirdList
    }
  }

  //own reverse function
  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def reverseIter(list: List[A], rev: List[A]): List[A] = {
      if (list == Nil) rev
      else reverseIter(list.tail, list.head :: rev)
    }
    reverseIter(list, Nil)
  }

  //own string length function
  def stringLength(string: String): Int = {
    @tailrec
    def stringLengthIter(string: String, length: Int): Int = {
      if (string == "") length
      else stringLengthIter(string.tail, length + 1)
    }
    stringLengthIter(string, 0)
  }

  //own contains function
  @tailrec
  final def contains[A](list: List[A], element: A): Boolean = {
    if (list == Nil) return false
    else if (list.head == element) return true
    else contains(list.tail, element)
  }
}
