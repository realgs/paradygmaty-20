class list2 {
  //zadanie 1
  def multiply(xs: List[Double]): Double =
    if (xs == Nil) 0
    else if (xs.tail == Nil) xs.head
    else xs.head * multiply(xs.tail)

  //zadanie 2
  def combineStrings(xs: List[String], separator: String, endSign: String): String =
    if (xs == Nil) endSign
    else if (xs.tail == Nil) xs.head + combineStrings(xs.tail, separator, endSign)
    else xs.head + separator + combineStrings(xs.tail, separator, endSign)

  //zadanie 3
  def checkNumbers(xs: List[Double], X: Double, Y: Double): Boolean =
    if (Y >= X)
      if (xs == Nil) true
      else if (xs.head >= X && xs.head <= Y) checkNumbers(xs.tail, X, Y)
      else false
    else throw new Exception("Y must be greater than X!")

  //zadanie 4
  def pow(base: Double, exponent: Int): Double =
    if (exponent < 0 && base == 0) throw new Exception("illegal expression")
    else if (exponent == 0) 1
    else if (exponent > 0) base * pow(base, exponent - 1)
    else 1 / base * pow(base, exponent + 1)

}