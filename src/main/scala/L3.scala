import scala.annotation.tailrec

object L3 extends App {
  // zadanie 1
  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def reverseHelper(list: List[A], reversedList: List[A]): List[A] = {
      if (list == Nil) reversedList
      else reverseHelper(list.tail, list.head :: reversedList)
    }

    reverseHelper(list, Nil)
  }

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
  def length[A](list: List[A]): Int = {
    if (list == Nil) 0
    else if (list.tail == Nil) 1
    else 1 + length(list.tail)
  }

  // zadanie 3
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
}
