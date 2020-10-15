import functions.Functions._
import org.scalactic.Tolerance.convertNumericToPlusOrMinusWrapper
import org.scalatest.flatspec.AnyFlatSpec

class FunctionsTests extends AnyFlatSpec {
  "Exercise 1 function" should "multiply floating point numbers of list correctly" in {
    assert(multiply(List(1, 2, 3)) == 6)
    assert(multiply(List(10, -2, 3)) == -60)
    assert(multiply(Nil) == 0)
    assert(multiply(List(1.2, 3, -100)) == -360)
    assert(multiply(List(0)) == 0)
  }

  "Exercise 2 function" should "return empty string for empty list" in {
    assert(createSentence(Nil, '.', ' ') == "")
  }

  "Exercise 2 function" should "create expected sentence" in {
    assert(createSentence(List("Ala", "ma", "kota"), '.', ' ') == "Ala ma kota.")
    assert(createSentence(List("SNAKE", "CASE", "CREATED"), '_', '_') == "SNAKE_CASE_CREATED_")
    assert(createSentence(List("OneWord"), '.', '_') == "OneWord.")
  }

  "Exercise 3 function" should "return true for empty list" in {
    assert(areInRange(Nil, 0, 10))
  }

  "Exercise 3 function" should "check if numbers are in range correctly" in {
    assert(areInRange(List(-5, 0, 2.5), -5.01, 2.51))
    assert(areInRange(List(0, 0, 0.1, -0.7), -0.7, 0.1))
    assert(!areInRange(List(-2, -1, 0, 1, 2), -1, 2))
    assert(!areInRange(List(-3, -1, 1, 3), -3, 2))
  }

  "Exercise 4 function" should "return 1 for exponent = 0" in {
    assert(power(1000, 0) == 1)
    assert(power(0, 0) == 1)
  }

  "Exercise 4 function" should "calculate power for positive exponents correctly" in {
    assert(power(1.5, 2) === 2.25 +- 0.01)
    assert(power(-0.1, 3) === -0.001 +- 0.01)
    assert(power(2, 10) == 1024)
  }

  "Exercise 4 function" should "always return 0 for base = 0 and exponent > 0" in {
    assert(power(0, 1000) == 0)
  }

  "Exercise 4 function" should "throw an exception if base = 0 and exponent < 0" in {
    assertThrows[Exception](power(0, -1))
  }

  "Exercise 4 function" should "calculater power for negative exponents correctly" in {
    assert(power(2, -3) == 0.125)
    assert(power(0.1, -2) == 100)
  }





}
