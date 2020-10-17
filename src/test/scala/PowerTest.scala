import org.scalatest.FunSuite

class PowerTest extends FunSuite{

  test("positiveExponent") {
    assert(L2.power(2,10) == 1024)
  }

  test("negativeExponent") {
    assert(L2.power(2,-10) == 1/1024.0)
  }

  test("zeroExponent") {
    assert(L2.power(10,0) == 1)
  }

  test("zeroToZeroExponent") {
    assertThrows[Exception] { L2.power(0,0) }
  }
}
