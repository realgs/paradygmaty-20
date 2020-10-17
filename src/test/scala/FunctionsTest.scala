import org.scalatest.funsuite.AnyFunSuite

class FunctionsTest extends AnyFunSuite {
  test("doubleProduct") {
    assert(Functions.doubleProduct(List(6.24, 3.78, 1235.9, 45.87)) === (6.24 * 3.78 * 1235.9 * 45.87))
    assert(Functions.doubleProduct(List(-5.2, -6.7, -8.2)) === (-5.2 * -6.7 * -8.2))
    assert(Functions.doubleProduct(List(23423468.43452, 5469894525.283443, 23445345.3656456)) ===
      (23423468.43452 * 5469894525.283443 * 23445345.3656456))
    assert(Functions.doubleProduct(List(1.0, -1.0, 0.0)) === 0.0)
    assert(Functions.doubleProduct(Nil) === 0.0)
    assert(Functions.doubleProduct(null) === 0.0)
  }

  test("combineTokens") {
    assert(Functions.combineTokens(List("I", "love", "it"), " ", '!') === "I love it!")
    assert(Functions.combineTokens(List("word1", "word2", "word3"), ", ", '.') === "word1, word2, word3.")
    assert(Functions.combineTokens(Nil, ",", '!') === "")
    assert(Functions.combineTokens(null, "", ' ') === "")

  }

  test("listInRange") {
    assert(Functions.listInRange(List(1), 1, 1))
    assert(Functions.listInRange(List(1, 2, 3, 4, 5), 1, 5))
    assert(!Functions.listInRange(List(5.5, 82.3, 900.8, 23.4, -5.2), 0, 1000))
    assert(Functions.listInRange(List(), 0, 1))
    assert(Functions.listInRange(null, -5, 10))
    assertThrows[IllegalArgumentException] {
      Functions.listInRange(List(1, 2, 3, 4), 4, 1)
    }
  }

  test("power") {
    assert(Functions.power(256.128, 0) === 1)
    assert(Functions.power(1234.567, 1) === 1234.567)
    assert(Functions.power(1, 1234) === 1)
    assert(Functions.power(12, 5) === math.pow(12, 5))
    assert(Functions.power(-8, 6) === math.pow(-8, 6))
    assertThrows[IllegalArgumentException] {
      Functions.power(0, 0)
    }
    assertThrows[IllegalArgumentException] {
      Functions.power(81, -2)
    }
  }

}
