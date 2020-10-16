package list_2

import scala.annotation.tailrec

object Functions {

  def calculateProduct(numbers: List[Double]): Double = {
    if (numbers == Nil) 0
    else if (numbers.tail == Nil) numbers.head
    else numbers.head * calculateProduct(numbers.tail)
  }

  def createSentence(stringList: List[String], separator: Char, endCharacter: Char): String = {
    if (stringList == Nil) ""
    else if (stringList.tail == Nil) stringList.head + endCharacter
    else stringList.head + separator + createSentence(stringList.tail,separator,endCharacter)
  }

  @tailrec
  def checkInRange(numberList: List[Double], lowerBoundary: Double, upperBoundary: Double): Boolean = {
    if(lowerBoundary > upperBoundary) throw new Exception("Incorrect Boundaries")
    else if(numberList == Nil) true
    else if(lowerBoundary <= numberList.head && numberList.head <= upperBoundary) checkInRange(numberList.tail,lowerBoundary,upperBoundary)
    else false
  }

  def power(base: Double, exponent: Int): Double = {
    if(base == 0.0 && exponent == 0)  throw new Exception("Undefined value")
    else if(exponent == 0) 1.0
    else if(base == 0.0) 0.0
    else if(exponent < 0) 1/base * power(base, exponent+1)
    else base * power(base,exponent-1)
  }


}
