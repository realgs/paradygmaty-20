import scala.annotation.tailrec

object Lista2 extends App {
  // Zadanie 1
  def multiplyRealNumbersList(numbers: List[Double]): Double = {
    if (numbers == Nil) 0
    else if (numbers.tail == Nil) numbers.head
    else numbers.head * multiplyRealNumbersList(numbers.tail)
  }

  println("Testy do zadania 1")
  println(multiplyRealNumbersList(List(1, 2, 3)) == 6)
  println(multiplyRealNumbersList(List(-1, 2, 3)) == -6)
  println(multiplyRealNumbersList(List(0.1, 0.2, 0.3)) == 0.006)
  println(multiplyRealNumbersList(List(-0.1, -0.2, -0.3)) == -0.006)
  println(multiplyRealNumbersList(List(0, 0.1, 2)) == 0)
  println(multiplyRealNumbersList(List()) == 0)

  /*
    * Zadanie 2
    * W tym zadaniu wyjątkowo zrobiłem obsługę nulli, ponieważ w treści zadań nie było to doprecyzowane
    * Nie zezwalam na nulle w liscie stringow, ale separator i koniec zdania moze nim byc
  */
  def createSentence(stringsList: List[String], stringsSeparator: String, sentenceEndMark: String): String = {
    if (stringsList == Nil) convertNullIntoEmptyString(sentenceEndMark)
    else throwExceptionOnNullValue(stringsList.head) +
      (if (stringsList.tail != Nil) convertNullIntoEmptyString(stringsSeparator) else "") +
      createSentence(stringsList.tail, stringsSeparator, sentenceEndMark)
  }

  private def convertNullIntoEmptyString(nullableValue: String) = if (nullableValue == null) "" else nullableValue

  private def throwExceptionOnNullValue[T](value: T): T = {
    if (value != null) value
    else throw new IllegalArgumentException("Given value cannot be null")
  }

  println("\nTesty do zadania 2")
  println(
    createSentence(
      stringsList = List("Ala", "ma", "kota"),
      stringsSeparator = " ",
      sentenceEndMark = "."
    ) == "Ala ma kota."
  )
  println(
    createSentence(
      stringsList = List("Ala", "ma", "kota"),
      stringsSeparator = null,
      sentenceEndMark = "."
    ) == "Alamakota."
  )
  println(
    createSentence(
      stringsList = List("Ala", "ma", "kota"),
      stringsSeparator = null,
      sentenceEndMark = null
    ) == "Alamakota"
  )
  println(
    createSentence(
      stringsList = List(),
      stringsSeparator = " ",
      sentenceEndMark = "!"
    ) == "!"
  )
  /* Tutaj zostanie zwrocony wyjatek
  println(
    createSentence(
      stringsList = List("Ala", null, "kota"),
      stringsSeparator = " ",
      sentenceEndMark = "."
    )
  ) */

  // Zadanie 3
  def checkIfNumbersAreInRange(numbers: List[Int], minValue: Int, maxValue: Int): Boolean = {
    if (maxValue < minValue) throw new IllegalArgumentException("maxValue should be greater than minValue")
    else if (numbers == Nil) false
    else checkNumbersInRange(numbers, minValue, maxValue)
  }

  @tailrec
  private def checkNumbersInRange(numbers: List[Int], minValue: Int, maxValue: Int): Boolean = {
    if (minValue to maxValue contains numbers.head) {
      if (numbers.tail == Nil) true
      else checkNumbersInRange(numbers.tail, minValue, maxValue)
    } else {
      false
    }
  }

  println("\nTesty do zadania 3")
  println(!checkIfNumbersAreInRange(numbers = List(1, 2, 3), minValue = 10, maxValue = 20))
  println(checkIfNumbersAreInRange(numbers = List(2), minValue = 1, maxValue = 3))
  println(checkIfNumbersAreInRange(numbers = List(1, 2, 3), minValue = 1, maxValue = 3))
  // println(checkIfNumbersAreInRange(numbers = List(1, 2, 3), minValue = 3, maxValue = 1)) - Zostanie zwrocony wyjatek

  // Zadanie 4
  def powerNumber(number: Double, exponent: Int): Double = {
    if (number == 0) {
      if (exponent < 0) throw new IllegalArgumentException("Cannot divide by 0")
      else if (exponent == 0) 1
      else 0
    }
    else if (exponent < 0) 1 / calculatePower(number, -1 * exponent)
    else calculatePower(number, exponent)
  }

  private def calculatePower(number: Double, exponent: Int): Double = {
    if (exponent <= 1) number
    else number * calculatePower(number, exponent - 1)
  }

  println("\nTesty do zadania 4")
  println(powerNumber(number = 1, exponent = 2) == 1)
  println(powerNumber(number = 0, exponent = 0) == 1)
  println(powerNumber(number = -1, exponent = -2) == 1)
  println(powerNumber(number = 2, exponent = -1) == 0.5)
  println(powerNumber(number = 0, exponent = 2) == 0)
  // println(powerNumber(number = 0, exponent = -1) == 0) Zostanie zwrocony wyjatek
}