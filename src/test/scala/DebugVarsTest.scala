import org.scalatest.FunSuite

// zadanie 4 testy (5 pkt)
class DebugVarsTest extends  FunSuite{

  test("basicTest") {
    val point = new Point(3,4)
    val printTest = new PrintTest("test", 12)

    point.debugVars()
    println()
    printTest.debugVars()
  }
}
