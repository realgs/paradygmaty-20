import scala.annotation.tailrec

object Functions {
  // Function 1
  def multiplyNumbers(numbers: List[Double]): Double =
    if (numbers == Nil) 0
    else if (numbers.tail == Nil) numbers.head
    else numbers.head * multiplyNumbers(numbers.tail)

  // Function 2
  def transformString(listOfStrings: List[String], endPunctuation: String, separator: String): String =
    if (listOfStrings == Nil) ""
    else {
      val punctuation = if (listOfStrings.tail != Nil) separator else endPunctuation
      listOfStrings.head + punctuation + transformString(listOfStrings.tail, endPunctuation, separator)
    }

  // Function 3
  @tailrec
  def withinRange(numbers: List[Double], startRange: Double, endRange: Double): Boolean =
    if (numbers == Nil) true
    else (numbers.head >= startRange && numbers.head <= endRange) && withinRange(numbers.tail, startRange, endRange)

  // Function 4
  // tylko w potegowaniu uzywam BigDecimal, poniewaz w tym przypadku liczby moga latwo urosnac 'duze'
  def power(base: Double, exponent: Int): BigDecimal = {
    if (exponent < 0 && base == 0) throw new Exception("Undefined value")
    if (exponent == 0) 1
    else if (exponent > 0) base * power(base, exponent - 1)
    else power(base, exponent + 1) / base
  }
}