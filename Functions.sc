import scala.annotation.tailrec

// Function 1
def multiplyNumbers(numbers: List[Double]): Double =
  if(numbers == Nil) 0
  else if(numbers.tail == Nil) numbers.head
  else numbers.head * multiplyNumbers(numbers.tail)

multiplyNumbers(Nil) == 0
multiplyNumbers(List(2)) == 2
multiplyNumbers(List(12.5, 0, 11, 10)) == 0
multiplyNumbers(List(1.5, 2.25, -2.75, 0.5, 4)) == -18.5625
multiplyNumbers(List(-12345.12345, 23456.23456)) <= -2.895701 * Math.pow(10, 8)

// Function 2
def transformString(listOfStrings: List[String], endPunctuation: String, separator: String): String =
  if(listOfStrings == Nil) ""
  else {
    val punctuation = if(listOfStrings.tail != Nil) separator else endPunctuation
    listOfStrings.head + punctuation + transformString(listOfStrings.tail, endPunctuation, separator)
  }

transformString(List(), "?", " ") == ""
transformString(List(""), "...", " ") == "..."
transformString(List("", "", ""), "!", ", ") == ", , !"
transformString(List("This", "is", "test"), ".", " ") == "This is test."
transformString(List("Unusu", "visu", "ritual"), "!", "al ") == "Unusual visual ritual!"

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
// tylko w potegowaniu uzywam BigDecimal, poniewaz w tym przypadku liczby moga latwo urosnac 'duze'
def power(base: Double, exponent: Int): BigDecimal = {
  if(exponent < 0 && base == 0) throw new Exception("Undefined value")
  if(exponent == 0) 1
  else if (exponent > 0) base * power(base, exponent - 1)
  else power(base, exponent + 1) / base
}

power(0, 1) == Math.pow(0, 1)
power(0, 0) == Math.pow(0, 0)
power(10, 0) == Math.pow(10, 0)
power(-2, 0) == Math.pow(-2, 0)
power(-2, 9) == Math.pow(-2, 9)
power(2, 9) == Math.pow(2, 9)
power(2.5, 4) == Math.pow(2.5, 4)
power(-1.5, 2) == Math.pow(-1.5, 2)
power(5, -3) == Math.pow(5, -3)

// dla bardziej szczegółowych wynikow Math.pow nie zadziala wystarczajaco dokladnie bo java nie uzywa BigDecimal w tej metodzie
// wynik z funkcji uzywajacej BigDecimal jest bardziej dokladny niz Math.pow wiec sprawdzam >=

power(123, 20) >= Math.pow(123, 20)
power(123.1123, 60) >= 2.61870571 * Math.pow(10, 125)
power(123, -20) >= 1.5918339803 * Math.pow(10, -42)
power(0, -1) // exception
//