object Functions {

  def doubleProduct(list:List[Double]): Double =
    if(list == Nil || list == null) 0
    else if(list.length == 1) list.head
    else list.head * doubleProduct(list.tail)

  def combineTokens(words:List[String], ending:String, separator:String): String =
    if(words.length == 1) words.head + ending
    else words.head + separator + combineTokens(words.tail, ending, separator)

  def listInRange(numbers:List[Double], low:Double, high:Double): Boolean =
    if(numbers == Nil) true
    else if(numbers.head < low || numbers.head > high) false
    else listInRange(numbers.tail, low, high)

  def power(base:Double, exponent:Int): Double =
    if(exponent != 0) base * power(base, exponent - 1)
    else 1

}
