import org.scalatest.FunSuite

class RandomTreeGeneratorTest extends FunSuite{

  test("requestedDepth") {
    assert(Utils.calculateDepth(Lista4.generateRandomTree(4,10,20)) == 4)
  }

  test("depthZero") {
    assert(Lista4.generateRandomTree(0,10,20) == Empty)
  }

  test("depthOne") {
    assert(Lista4.generateRandomTree(1,1,1) == Node(1, Empty, Empty))
  }

  test("negativeBounds") {
    assertThrows[IllegalArgumentException](Lista4.generateRandomTree(4,-9,1))
  }

  test("lowerBoundGreaterThanUpper") {
    assertThrows[IllegalArgumentException](Lista4.generateRandomTree(4,9,1))
  }

}
