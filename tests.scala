import org.junit.jupiter.api.{Assertions, BeforeAll, Test}

class tests {
  val testList = List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224")
  val testFunction = new Functions

  @Test def testFunction1: Unit = {
    assert(testFunction.split(List(-3, -6, 8, -9, 13)) == (List(-3, -6, -9), List(-3, -9)))
    assert(testFunction.split(List()) == (List(), List()))
    assert(testFunction.split(List(-1, -2, -3, -4, -5, -6, -7, 1, 2, 3)) == (List(-1, -2, -3, -4, -5, -6, -7), List(-1, -3, -5, -7)))
    assert(testFunction.split(List(1, 2, 3)) == (List(), List()))
    assert(testFunction.split(List(-1)) == (List(-1), List(-1)))
  }

}
