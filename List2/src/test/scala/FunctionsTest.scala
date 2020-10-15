import org.scalatest.FunSuite

class FunctionsTest extends FunSuite {
  // Zadanie 1
  test("cumulativeProduct.null") {
    assert(Functions.cumulativeProduct(null) === 0)
  }

  test("cumulativeProduct.empty") {
    assert(Functions.cumulativeProduct(List()) === 0)
  }

  test("cumulativeProduct.simpleInt") {
    assert(Functions.cumulativeProduct(List(5, 2, 9, 1, 2, 8, 5)) === 7200)
  }

  test("cumulativeProduct.simpleDouble") {
    assert(Math.abs(Functions.cumulativeProduct(List(3.14, 8.11, 2.999, 0.004, 103.5)) - 31.6174841244) < 0.000001)
  }

  test("cumulativeProduct.negativeInt") {
    assert(Functions.cumulativeProduct(List(-7, -3, 2, -9, 5, -4, -2)) === -15120)
  }

  test("cumulativeProduct.bigIntToDoubleCoercion") {
    assert(Functions.cumulativeProduct(List(1 << 30, 1 << 30)) === (1 << 30).toDouble * (1 << 30).toDouble)
  }


  // Zadanie 2
  test("concatenateStrings.null") {
    assertThrows[IllegalArgumentException] {
      Functions.concatenateStrings(List("first", "second", "third"), null, 'c')
    }
  }

  test("concatenateStrings.emptyList") {
    assert(Functions.concatenateStrings(List(), ",", 'c') === "")
  }

  test("concatenateStrings.oneElementList") {
    assert(Functions.concatenateStrings(List("AAA"), ",", 'c') === "AAAc")
  }

  test("concatenateStrings.simpleList") {
    assert(
      Functions.concatenateStrings(List("first", "second", "third"), ",", '#') ===
        "first,second,third#"
    )
  }


  // Zadanie 3
  test("areIn.nullList") {
    assert(!Functions.areIn(null, 10, 15))
  }

  test("areIn.simpleInt") {
    assert(Functions.areIn(List(7, 39, 4, 24, 38, 26, 44, 12, 47), 1, 50))
  }

  test("areIn.simpleDouble") {
    assert(Functions.areIn(List(0.333, 0.9999999999, 0.564, 0.0000001, 0.783), 0, 1))
  }

  test("areIn.negative") {
    assert(Functions.areIn(List(-14.5909, 10, 7, -24, 19.31425, 9.9999), -25, 25))
  }

  test("areIn.oneValueNotInRange") {
    assert(!Functions.areIn(List(-14.5909, 10, 7, -27, 19.31425, 9.9999), -25, 25))
  }

  test("areIn.noValueInRange") {
    assert(!Functions.areIn(List(18, 27.34, 30, 32, 25, 11.99, -31, 29, 25.657, -14), 0, 10))
  }

  test("areIn.rangeBoundsNotInOrder") {
    assert(Functions.areIn(List(9, 4, 6, 6, 5, 10, 1), 10, 0))
  }


  // Zadanie 4
  test("exponent.zeroBase") {
    assert(Functions.exponent(0, 15) === 0)
  }

  test("exponent.undefined") {
    assertThrows[IllegalArgumentException] {
      Functions.exponent(0, 0)
    }
  }

  test("exponent.positiveBase") {
    assert(Functions.exponent(9.5, 3) === 857.375)
  }

  test("exponent.negativeBase") {
    assert(Math.abs(Functions.exponent(-3.14, 5) - -305.244776182) < 0.000001)
  }

  test("exponent.negativeExponent") {
    assert(Functions.exponent(1.0 / 2, -9) === 512)
  }

  test("exponent.maxExponent") {
    assert(Functions.exponent(1.0, Integer.MAX_VALUE) === 1)
  }

  test("linearExponentDemo.maxExponent") {
    assertThrows[StackOverflowError] {
      Functions.linearExponentDemo(1.0, Integer.MAX_VALUE)
    }
  }
}
