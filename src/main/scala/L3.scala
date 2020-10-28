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
    def divideListHelper(list: List[Int], negativeNumbersList: List[Int], negativeOddNumbersList: List[Int]): (List[Int], List[Int]) = {
      if (list == Nil) (negativeNumbersList, negativeOddNumbersList)
      else if (list.head < 0) {
        if (list.head % 2 != 0) divideListHelper(list.tail, list.head :: negativeNumbersList, list.head :: negativeOddNumbersList)
        else divideListHelper(list.tail, list.head :: negativeNumbersList, negativeOddNumbersList)
      }
      else divideListHelper(list.tail, negativeNumbersList, negativeOddNumbersList)
    }

    divideListHelper(reverse(list), Nil, Nil)
  }
}
