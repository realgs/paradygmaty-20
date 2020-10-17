object L2 {

  // Zadanie 1
  def multiplyListElements(list: List[Double]): Double = {
    if (list == Nil) 0
    else if (list.tail == Nil) list.head
    else list.head * multiplyListElements(list.tail)
  }

  // Zadanie 2
  def concatenateWords(words: List[String], separator: String, endCharacter: Char): String = {
    if (words == Nil) endCharacter.toString
    else if (words.tail == Nil) words.head + endCharacter
    else words.head + separator + concatenateWords(words.tail, separator, endCharacter)
  }

  // Zadanie 3
  def areAllElementsInRange(elements: List[Double], startRange: Double, endRange: Double): Boolean = {
    if (endRange < startRange) throw new Exception("End range cannot be smaller than start range")

    if (elements == Nil) true
    else if (elements.head >= startRange && elements.head <= endRange) {
      if (elements.tail == Nil) true
      else areAllElementsInRange(elements.tail, startRange, endRange)
    }
    else false
  }

  // Zadanie 4
  def power(base: Double, exponent: Int): Double = {
    if (base == 0) {
      if (exponent < 0) throw new Exception("This value cannot be computed")
      else if (exponent == 0) 1
      else 0
    }
    else if (exponent > 0) base * power(base, exponent - 1)
    else if (exponent < 0) 1 / base * power(base, exponent + 1)
    else 1
  }

}
