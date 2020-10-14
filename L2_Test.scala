import org.scalatest.funsuite.AnyFunSuite

class L2_Test extends AnyFunSuite {

  val l2 = new L2

  test("Test integers; multiplayRealNumb function") {
    assert(l2.multiplyRealNumb(List(10, 6, 2)) == 120)
    assert(l2.multiplyRealNumb(List(1, -2, 3, 4)) == -24)
    assert(l2.multiplyRealNumb(List(-3, -10)) == 30)
  }

  test("Test real numbers; multiplayRealNumb function") {
    assert(l2.multiplyRealNumb(List(2.5, 4.0, 1.22)) == 12.2)
    assert(l2.multiplyRealNumb(List(-2.4, 2)) == -4.8)
  }

  test("Test one-element list; multiplayRealNumb function") {
    assert(l2.multiplyRealNumb(List(3.44)) == 3.44)
  }

  test("Test empty list; multiplayRealNumb function") {
    assert(l2.multiplyRealNumb(List()) == 0)
  }

  test("Test building sentences; createSentence function") {
    assert(l2.createSentence(List("Ania", "ma", "kota"), '.', ' ').equals("Ania ma kota."))
  }

  test("Test one-element list; createSentence function") {
    assert(l2.createSentence(List("Hej"), '!', ' ').equals("Hej!"))
  }

  test("Test empty list; createSentence function") {
    assert(l2.createSentence(Nil, '?', ' ').equals("?"))
  }

  test("Test integers; isInInterval function") {
    assert(l2.isInInterval(List(2, -5, 9, -3), -6, 10))
  }

  test("Test real numbers; isInInterval function") {
    assert(l2.isInInterval(List(2.4, -5.9, 9.2, -3.2), -6.4, 10.1))
  }

  test("Test empty list; isInInterval function") {
    assert(l2.isInInterval(List(), -6, 10))
  }

  test("Test exception when lower>upper; isInInterval function") {
    assertThrows[Exception] {
      l2.isInInterval(List(2, -5), 4, 2)
    }
  }

  test("Test positive exponent; exponentiate function") {
    assert(l2.exponentiate(1.2, 2) == 1.44)
    assert(l2.exponentiate(-2, 5) == -32)
  }

  test("Test negative exponent; exponentiate function") {
    assert(l2.exponentiate(0.2, -2) == 25)
  }

  test("Test exponent == 0; exponentiate function") {
    assert(l2.exponentiate(4, 0) == 1)
  }

  test("Test base == 0; exponentiate function") {
    assert(l2.exponentiate(0, 4) == 0)
  }

}
