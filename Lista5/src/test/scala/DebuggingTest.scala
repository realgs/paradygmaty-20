import Debugging.Point
import org.scalatest.FunSuite

class DebuggingTest extends FunSuite {

  val p : Point = new Point(3,4)

  //Zad 3 test
  test("debugName.point") {
    p.debugName()
  }

  //Zad 4 test
  test("debugVars.point") {
    p.debugVars()
  }

  //Zad 5 tests
  test("getDebugName.point") {
    assert(p.getDebugName == "Point")
  }
  test("getDebugVars.point") {
    val resultArray = p.getDebugVars
    for (field <- resultArray) {
      println("Var: " + field._1 + " => " + field._2 + ", " + field._3)
    }
  }
  test("getDebugVarsString.point") {
    assert(p.getDebugVarsString === Array(("x", "int", "3"), ("y", "int", "4"), ("a", "class java.lang.String", "test")))
  }
}
