import scala.annotation.tailrec

// Function 1
def multiplyNumbers(numbers: List[Double]): Double =
  if(numbers == Nil) 0
  else
    if(numbers.tail == Nil) numbers.head
    else numbers.head * multiplyNumbers(numbers.tail)

multiplyNumbers(Nil) == 0
multiplyNumbers(List(2)) == 2
multiplyNumbers(List(12.5, 0, 11, 10)) == 0
multiplyNumbers(List(1,2,3,4,5)) == 120
multiplyNumbers(List(1.5, 2.25, -2.75, 0.5, 4)) == -18.5625

// Function 2
def transformString(listOfStrings: List[String], endPunctuation: String, separator: String): String = {
  if(listOfStrings == Nil) ""
  else {
    val punctuation = if(listOfStrings.tail != Nil) separator else endPunctuation
    s"${listOfStrings.head}" + punctuation + transformString(listOfStrings.tail, endPunctuation, separator)
  }
}

transformString(List(), "?", " ")
transformString(List(""), "...", " ")
transformString(List("", "", ""), "!", ", ")
transformString(List("This", "is", "test"), ".", " ")
transformString(List("Unusu", "visu", "ritual"), "!", "al ")

// Function 3
@tailrec
def withinRange(numbers: List[Double], startRange: Double, endRange: Double): Boolean =
  if(numbers == Nil) true
  else (numbers.head >= startRange && numbers.head <= endRange) && withinRange(numbers.tail, startRange, endRange)

withinRange(List(), -2, 4)
withinRange(List(-1, 1, 2, 3, 4), -1, 5)
withinRange(List(-125.5, -1.2, 0, 2, 4.25), -125.5, 4.25)
withinRange(List(-1, 0, 1), -1, 1)
withinRange(List(-1, -2, 0, 2), -1, 1)

// Function 4
def power(base: Double, exponent: Int): BigDecimal = {
  @tailrec
  def subPower(result: Double, exponent: Int): BigDecimal = exponent match {
    case 0 => 1
    case 1 => result
    case _ => subPower(result * base, exponent - 1)
  }
  subPower(base, exponent)
}

power(0, 1) == 0
power(0,0) == 1
power(10, 0) == 1
power(10, 1) == 10
power(-2,9) == -512
power(2,9) == 512
power(2.5, 2) == 6.25
power(-1.5, 2) == 2.25
power(123,8)
power(123.1123, 60)
