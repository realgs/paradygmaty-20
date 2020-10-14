package List2

object L2 {
  val product: List[Double] => Double = (list: List[Double]) =>
    if(list == Nil) 0
    else if(list.tail != Nil) list.head * product(list.tail)
    else list.head

  val makeSentence: (List[String], Char, Char) => String = (list: List[String], separator: Char, endCharacter: Char) =>
    if(list == Nil) ""
    else if(list.tail != Nil) list.head + separator + makeSentence(list.tail, separator, endCharacter)
    else list.head + endCharacter

  val isInRange: (List[Double], Double, Double) => Boolean = (list: List[Double], leftValue: Double, rightValue: Double ) =>
    if(leftValue > rightValue) throw new Exception("Incorrect range")
    else if(list == Nil) true
    else if(list.tail != Nil) {
      if(list.head >= leftValue && list.head <= rightValue) isInRange(list.tail, leftValue, rightValue)
      else false
    }
    else {
      if (list.head >= leftValue && list.head <= rightValue) true
      else false
    }

  val power: (Double, Int) => Double = (base: Double, index: Int) =>
    if(index == 0) 1
    else if(base == 0) 0
    else if(index < 0) 1/base * power(base, index+1)
    else base * power(base, index-1)
}
