import scala.annotation.tailrec

object Lists {

  //własna interpretacja funkcji reverse
  //Złożoność obliczeniowa O(n), gdzie n = długość listy, złożoność pamięciowa O(1) - zoptymalizowana rekursja ogonowa
  def listReverse[A] (list: List[A]): List[A] = {
    @tailrec
    def listReverseTail(list: List[A], reverseList: List[A]): List[A] = {
      if(list == Nil) reverseList
      else listReverseTail(list.tail, list.head::reverseList)
    }
    listReverseTail(list, Nil)
  }

  //Zad 1
  //Złożoność obliczeniowa O(n), gdzie n = długość listy + O(k) + O(j) ze względu na odwracanie list wynikowych, złożoność pamięciowa O(1) - zoptymalizowana rekursja ogonowa
  def findNegativeAndOddNegative (list: List[Double]): (List[Double], List[Double]) = {
    @tailrec
    def findNegativeAndOddNegativeTail (list: List[Double], negative: List[Double], oddNegative: List[Double]): (List[Double], List[Double]) = {
      list match {
        case Nil => (listReverse(negative), listReverse(oddNegative))
        case h::t => findNegativeAndOddNegativeTail(t, if(h < 0) h::negative else negative, if(h < 0 && h % 2 != 0) h:: oddNegative else oddNegative)
      }
    }
    findNegativeAndOddNegativeTail(list, Nil, Nil)
  }

  //Zad 2
  //Złożoność obliczeniowa O(n), gdzie n = długość listy, złożoność pamięciowa O(1) - zoptymalizowana rekursja ogonowa
  def listLenght[A] (list: List[A]): Int = {
    @tailrec
    def listLengthTail(list: List[A], length: Int): Int = {
      if (list == Nil) length
      else listLengthTail(list.tail, length + 1)
    }
    listLengthTail(list, 0)
  }

  //Zad 3
  //Złożoność obliczeniowa O(n), gdzie n = sumaryczna długość list + O(n) na odwrócenie listy wynikowej, złożoność pamięciowa O(1) - zoptymalizowana rekursja ogonowa
  def mergeTwoListsAlternately[A] (list1: List[A], list2: List[A]): List[A] = {
    @tailrec
    def mergeTwoListsAlternatelyTail(list1: List[A], list2: List[A], mergedList: List[A]): List[A] = {
      (list1, list2) match {
        case (Nil, Nil) => listReverse(mergedList)
        case (Nil, h::t) => mergeTwoListsAlternatelyTail(Nil, t, h::mergedList)
        case (h::t, Nil) => mergeTwoListsAlternatelyTail(t, Nil, h::mergedList)
        case (h1::t1, h2::t2) => mergeTwoListsAlternatelyTail(t1, t2, h2::h1::mergedList)
      }
    }
    mergeTwoListsAlternatelyTail(list1, list2, Nil)
  }

  //Zad 5
  //Złożoność obliczeniowa O(n), gdzie n = sumaryczna długość 3 list, złożoność pamięciowa O(n) - n wywołań na stosie programu
  def mergeThreeListsRec[A] (list1: List[A], list2: List[A], list3: List[A]): List[A] = {
    (list1, list2, list3) match {
      case (Nil, Nil, Nil) => Nil
      case (h::t, _, _) => h::mergeThreeListsRec(t, list2, list3)
      case (_, h::t, _) => h::mergeThreeListsRec(Nil, t, list3)
      case (_, _, h::t) => h::mergeThreeListsRec(Nil, Nil, t)
    }
  }

  //Złożoność obliczeniowa O(n), gdzie n = sumaryczna długość 3 list, złożoność pamięciowa O(1) - zoptymalizowana rekursja ogonowa
  def mergeThreeLists[A] (list1: List[A], list2: List[A], list3: List[A]): List[A] = {
    @tailrec
    def mergeThreeListsTail(list1: List[A], list2: List[A], list3: List[A], mergedList: List[A]): List[A] = {
      (list1, list2, list3) match {
        case (Nil, Nil, Nil) => listReverse(mergedList)
        case (h::t, _, _) => mergeThreeListsTail(t, list2, list3, h::mergedList)
        case (_, h::t, _) => mergeThreeListsTail(Nil, t, list3, h::mergedList)
        case (_, _, h::t) => mergeThreeListsTail(Nil, Nil, t, h::mergedList)
      }
    }
    mergeThreeListsTail(list1, list2, list3, Nil)
  }

}
