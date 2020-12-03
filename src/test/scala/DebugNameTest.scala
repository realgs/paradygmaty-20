import org.scalatest.FunSuite

// zadanie 3 testy (5 pkt)
class DebugNameTest extends FunSuite {

  test("basicTest") {
    val point = new Point(3,4)
    val printTest = new PrintTest("test", 12)

    point.debugName()
    printTest.debugName()
  }
}
