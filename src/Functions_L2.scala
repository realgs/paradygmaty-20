object Functions_L2 {
  def listMultiplication(list: List[Double]): Double = {
    if (list == Nil) 1
    else list.head * listMultiplication(list.tail)
  }

  def sentenceCreator(text: List[String], endMark: Char, separator: Char): String = {
    if (text == Nil) ""
    else if(text.length == 1) text.head + endMark
    else text.head + separator + sentenceCreator(text.tail, endMark, separator)
  }

  def numbersInRange(numbers: List[Double], lowerNumber: Double, higherNumber: Double): Boolean = {
    if (numbers == Nil) throw new Exception("Given list is empty - cannot compare to range")
    else if (lowerNumber > higherNumber) throw new Exception("Wrong range - lower number cannot be greater than higher one")
    else for (i <- numbers) {
      if (lowerNumber > i || higherNumber < i) return false
    }
    true
  }

  def pow(base: BigDecimal, exponent: Int): BigDecimal = {
    if (base == 0 && exponent <= 0) throw new Exception("Invalid values")
    else if (base == 0) 0
    else if (exponent < 0) positiveExponentPow(1 / base, -exponent)
    else positiveExponentPow(base, exponent)
  }

  private def positiveExponentPow(base: BigDecimal, exponent: Int): BigDecimal = {
    if (exponent == 0) 1
    else base * positiveExponentPow(base, exponent - 1)
  }
}
