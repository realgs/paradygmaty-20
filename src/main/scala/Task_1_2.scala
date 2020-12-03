object Task_1_2{
  //zadanie 1 (2.5 pkt)
  def repeat[A](listOfElem:List[A], listOfCounters: List[Int] ): List[A] = {
    def repeatIter(listOfElemInner: List[A], listOfCountersInner: List[Int], currentCounter: Int): List[A] = {
      (currentCounter) match {
        case 0 => repeat(listOfElem.tail, listOfCounters.tail)
        case _ => listOfElemInner.head :: repeatIter(listOfElemInner, listOfCountersInner, currentCounter - 1)
      }
    }

    if (listOfElem != Nil) (listOfCounters) match {
      case Nil => Nil
      case head :: _ => repeatIter(listOfElem, listOfCounters, head)
    } else Nil
  }

  //zadanie 2 (2.5 pkt)
  def repeateWithoutDublicates[A](listOfElem:List[A], listOfCounters: List[Int]): List[A] = {
    def repeatIter(listOfElem1: List[A], listOfCounters1: List[Int], currentCounter: Int): List[A] = {
      (currentCounter) match {
        case 0 => repeateWithoutDublicates(listOfElem.tail, listOfCounters.tail)
        case _ => listOfElem1.head :: repeatIter(listOfElem1, listOfCounters1, currentCounter - 1)
      }
    }

    if (listOfElem == Nil) Nil
    else if (!isUnique(listOfElem)) throw new Exception("First list contains dublicates")
    else (listOfCounters) match {
      case Nil => Nil
      case head :: _ => repeatIter(listOfElem, listOfCounters, head)
    }
  }

  def isUnique[A](list:List[A]): Boolean = {
    if (list != Nil) {
      val set = list.toSet
      if (list.length == set.size) true
      else false
    }
    else false
  }
}
