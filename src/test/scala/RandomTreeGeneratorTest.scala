import org.scalatest.FunSuite

// testy do zadania 1 (3 punkty)
class RandomTreeGeneratorTest extends FunSuite{

  test("requestedDepth") {
    val randomTree = Lista4.generateRandomTree(4,10,20)
    assert(Utils.calculateDepth(randomTree) == 4)
    assert(Utils.isTreeFull(randomTree))
  }

  test("depthZero") {
    val randomTree = Lista4.generateRandomTree(0,10,20)
    assert(randomTree == Empty)
    assert(Utils.isTreeFull(randomTree))
  }

  test("depthOne") {
    val randomTree = Lista4.generateRandomTree(1,1,1)
    assert(randomTree == Node(1, Empty, Empty))
    assert(Utils.isTreeFull(randomTree))
  }

  test("negativeDepth") {
    assertThrows[IllegalArgumentException](Lista4.generateRandomTree(-10,10,20))
  }

  test("negativeBounds") {
    assertThrows[IllegalArgumentException](Lista4.generateRandomTree(4,-9,1))
  }

  test("lowerBoundGreaterThanUpper") {
    assertThrows[IllegalArgumentException](Lista4.generateRandomTree(4,9,1))
  }

}
