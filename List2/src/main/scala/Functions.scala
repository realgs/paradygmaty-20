import scala.annotation.tailrec

object Functions {
  // Zadanie 1
  private def cumulativeProductRec(numbers: List[Double]): Double = {
    if (numbers == Nil) 1
    else numbers.head * cumulativeProductRec(numbers.tail)
  }

  def cumulativeProduct(numbers: List[Double]): Double = {
    if (numbers == null || numbers == Nil) 0
    else cumulativeProductRec(numbers)
  }

  // Zadanie 2
  private def concatenateStringsRec(words: List[String], separator: String, endOfLineCharacter: Char): String = {
    if (words == Nil) ""
    else if (words.length == 1) words.head + endOfLineCharacter.toString
    else words.head + separator + concatenateStringsRec(words.tail, separator, endOfLineCharacter)
  }

  def concatenateStrings(words: List[String], separator: String, endOfLineCharacter: Char): String = {
    if (words == null || separator == null) {
      throw new IllegalArgumentException("Can't concatenate null values");
    } else concatenateStringsRec(words, separator, endOfLineCharacter)
  }

  // Zadanie 3
  private def areInRec(numbers: List[Double], lowerBound: Double, upperBound: Double): Boolean = {
    if (numbers == Nil) true
    else {
      if (lowerBound <= numbers.head && numbers.head <= upperBound) {
        areInRec(numbers.tail, lowerBound, upperBound)
      } else false
    }
  }

  def areIn(numbers: List[Double], lowerBound: Double, upperBound: Double): Boolean = {
    if (numbers == null || numbers == Nil) return false

    if (lowerBound > upperBound) areInRec(numbers, upperBound, lowerBound)
    else areInRec(numbers, lowerBound, upperBound)
  }

  // Zadanie 4
  def exponent(base: Double, exponent: Int): Double = {
    @tailrec
    def auxExponent(exponent: Int, result: Double, power: Double): Double = {
      if (exponent <= 0) result
      else {
        if (exponent % 2 == 1) auxExponent(exponent >> 1, result * power, power * power)
        else auxExponent(exponent >> 1, result, power * power)
      }
    }

    if (exponent >= 0) {
      auxExponent(exponent, 1, base)
    } else {
      auxExponent(-exponent, 1, 1 / base)
    }
  }

  def linearExponentDemo(base: Double, exponent: Int): Double = {
    if (exponent == 0) 1
    else {
      base * linearExponentDemo(base, exponent - 1)
    }
  }
}
