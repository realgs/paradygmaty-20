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
}
