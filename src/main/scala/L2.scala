object L2 {

  // Zadanie 1
  def multiplyListElements(list: List[Double]) : Double = {
    if(list == Nil) 0
    else if(list.tail == Nil) list.head
    else list.head * multiplyListElements(list.tail)
  }

  // Zadanie 2
  def concatenateWords(words: List[String], separator: Char, endCharacter: Char) : String = {
    if(words == Nil) endCharacter.toString
    else if(words.tail == Nil) words.head + endCharacter
    else words.head + separator + concatenateWords(words.tail, separator, endCharacter)
  }

  // Zadanie 3
  def areAllElementsInRange(elements: List[Double], startRange: Double, endRange: Double ) : Boolean = {
    if(endRange < startRange) throw new Exception("End range cannot be smaller than start range")

    if(elements == Nil) true
    else if(elements.head >= startRange && elements.head <= endRange) {
      if(elements.tail == Nil) true
      else areAllElementsInRange(elements.tail, startRange, endRange)
    }
    else false
  }

  // Zadanie 4
  def power(x: Double, y: Int) : Double = {
    if(x == 0 && y == 0) throw new Exception("0 to 0th power cannot be computed")

    if (y > 0) x * power(x, y - 1)
    else if(y < 0) 1 / x * power(x, y + 1)
    else 1
  }

}
