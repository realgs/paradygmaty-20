import org.scalatest.FunSuite

class FunctionTest extends FunSuite {

  //Function 1 tests
  test("calculateProduct.empty"){
    assert(Functions.calculateProduct(List()) === 0.0)
  }

  test("calculateProduct.integerValues"){
    assert(Functions.calculateProduct(List(2,5,8,3,1)) === 240)
  }

  test("calculateProduct.doublePrecisionValues"){
    assert(Functions.calculateProduct(List(2.5,5.4,8.7,3.9,1.1)) - 503.8605 < 0.000001)
  }

  test("calculateProduct.negativeValues"){
    assert(Functions.calculateProduct(List(2.5,-5,-8.7,-3.9,1)) === -424.125)
  }

  test("calculateProduct.bigValues"){
    assert(Functions.calculateProduct(List(250,50000,80,390,1000000)) === 390000000000000000L)
  }

  test("calculateProduct.zeroValues"){
    assert(Functions.calculateProduct(List(2,5,8,3,0)) === 0)
  }

  //Function 2 tests
  test("createSentence.empty"){
    assert(Functions.createSentence(List(),' ','!') === "")
  }

  test("createSentence.oneString"){
    assert(Functions.createSentence(List("One"),' ','^') === "One^")
  }

  test("createSentence.fullSentence"){
    assert(Functions.createSentence(List("It","is","a","full","sentence"),' ','.') === "It is a full sentence.")
  }

  //Function 3 tests
  test("checkInRange.emptyList"){
    assert(Functions.checkInRange(List(),5.7,10.7))
  }

  test("checkInRange.incorrectBoundaries"){
    assertThrows[Exception]{Functions.checkInRange(List(1,2,3),10,5)}
  }

  test("checkInRange.integerListInRange"){
    assert(Functions.checkInRange(List(6,3,7,23,9,1),0.7,23))
  }

  test("checkInRange.doubleListInRange"){
    assert(Functions.checkInRange(List(6.2,3.78,7.23,23.12,9.2,1.1),1.1,23.12))
  }

  test("checkInRange.integerListOneOutOfBoundaries"){
    assert(!Functions.checkInRange(List(6,3,7,23,9,10),3.7,23))
  }

  test("checkInRange.doubleListAllOutOfBoundaries"){
    assert(!Functions.checkInRange(List(6.2,3.78,7.23,23.12,9.2,1.1),111.1,323.12))
  }
  test("checkInRange.doubleListInRangeWithNegativeValues"){
    assert(Functions.checkInRange(List(-6.2,3.78,7.23,-23.12,9.2,1.1),-23.12,9.2))
  }

  //Function 4 tests
  test("power.0^0"){
    assertThrows[Exception]{Functions.power(0,0)}
  }

  test("power.undefined"){
    assertThrows[Exception]{Functions.power(0,-1)}
  }

  test("power.exponent0"){
    assert(Functions.power(12.6,0) === 1)
  }

  test("power.base0") {
    assert(Functions.power(0, 12) === 0)
  }

  test("power.base1"){
    assert(Functions.power(1,1000000000) === 1)
  }

  test("power.basePositive"){
    assert((Functions.power(12.6,3) - 2000.376).abs < 0.000001)
  }

  test("power.baseNegative"){
    assert((Functions.power(-12.6,3) - -2000.376).abs < 0.000001)
  }

  test("power.exponentNegative"){
    assert(Functions.power(12.6,-3) > 0.00049)
  }
  test("power.bigNumber"){
    assert(Functions.power(10,15) === 1000000000000000L)
  }

}
