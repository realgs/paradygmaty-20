package List4

object L4LazyLists {

  //Zadanie 4(5pkt)
  def eachNElement[A](list: LazyList[A], n: Int, m: Int): LazyList[A] = {
    def eachNElementIter(list: LazyList[A], skip: Int, endIndex: Int): LazyList[A] =
      (list, skip, endIndex) match {
        case (LazyList(), _, _) => LazyList()
        case (h #:: _, _, 0) => if (skip == n && skip > 0) LazyList(h) else if (skip > 0) LazyList() else throw new Exception("Invalid data")
        case (h #:: t, skip, endIndex) => if (endIndex < 0) throw new Exception("Invalid data")
                                          else if (skip == n) h #:: eachNElementIter(t, 1, endIndex-1)
                                          else eachNElementIter(t, skip+1, endIndex-1)
      }
    eachNElementIter(list, n, if (m == 0) m else m-1)
  }

  //Zadanie 5(5pkt)
  val + : (Double, Double) => Double = (number1,number2) => number1 + number2
  val - : (Double, Double) => Double = (number1,number2) => number1 - number2
  val * : (Double, Double) => Double = (number1,number2) => number1 * number2
  val / : (Double, Double) => Double = (number1,number2) => number1 / number2

  def loperation(list1: LazyList[Double], list2: LazyList[Double])(operation: (Double, Double) => Double): LazyList[Double] =
    (list1, list2) match {
      case (LazyList(),LazyList()) => LazyList()
      case (LazyList(), _) => list2
      case (_, LazyList()) => list1
      case (h1 #:: t1, h2 #:: t2) => if ((operation == /) & (h2 == 0)) throw new Exception("You can't devide by 0!")
                                      else operation(h1,h2) #:: loperation(t1,t2)(operation)
    }

}