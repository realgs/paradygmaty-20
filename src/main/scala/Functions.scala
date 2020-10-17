object Functions {

  def doubleProduct(list: List[Double]): Double =
    if (list == Nil || list == null) 0
    else if (list.length == 1) list.head
    else list.head * doubleProduct(list.tail)

  def combineTokens(words: List[String], separator: String, ending: Char): String =
    if (words == Nil || words == null) ""
    else if (words.length == 1) words.head + ending
    else words.head + separator + combineTokens(words.tail, separator, ending)

  @scala.annotation.tailrec
  def listInRange(numbers: List[Double], low: Double, high: Double): Boolean =
    if (numbers == Nil || numbers == null) true
    else if(low > high) throw new IllegalArgumentException("Incorrect range: low should be < than high")
    else if (numbers.head < low || numbers.head > high) false
    else listInRange(numbers.tail, low, high)

  def power(base: Double, exponent: Int): Double =
    if (exponent < 0) throw new IllegalArgumentException("This function doesn't support exponents < 0!")
    else if(base == 0 && exponent == 0) throw new IllegalArgumentException("Undefined result for base = 0 and exponent = 0")
    else if (exponent != 0) base * power(base, exponent - 1)
    else 1

}
