import org.scalatest.funsuite.AnyFunSuite
import scala.collection.immutable.Queue

class FunctionsTest extends AnyFunSuite {

  test("duplicate") {
    assertThrows[NullPointerException](Functions.duplicate(null, List()))
    assertThrows[NullPointerException](Functions.duplicate(List(), null))

    assert(Functions.duplicate(List(), List()) === Queue())
    assert(Functions.duplicate(List(1), List()) === Queue())
    assert(Functions.duplicate(List(), List(1)) === Queue())
    assert(Functions.duplicate(List(1), List(3)) === Queue(1, 1, 1))
    assert(Functions.duplicate(List(1, 2), List(3)) === Queue(1, 1, 1))
    assert(Functions.duplicate(List(1, 2), List(3, 2, 1)) === Queue(1, 1, 1, 2, 2))
    assert(Functions.duplicate(List(1, 2, 3), List(0, 3, 1, 4)) === Queue(2,2,2,3))
    assert(Functions.duplicate(List(1, 2, 2, 3), List(0, 3, 2, 1, 4)) === Queue(2, 2, 2, 2, 2, 3))
    assert(Functions.duplicate(List(3, 3, 3), List(1, 2, 3)) === Queue(3, 3, 3, 3, 3, 3))
    assert(Functions.duplicate(List(1, 2, 3), List(-1, -2, -3)) === Queue())
  }

  test("duplicateDistinct") {
    assertThrows[NullPointerException](Functions.duplicateDistinct(null, List()))
    assertThrows[NullPointerException](Functions.duplicateDistinct(List(), null))

    assert(Functions.duplicateDistinct(List(), List()) === Queue())
    assert(Functions.duplicateDistinct(List(1), List()) === Queue())
    assert(Functions.duplicateDistinct(List(), List(1)) === Queue())
    assert(Functions.duplicateDistinct(List(1), List(3)) === Queue(1, 1, 1))
    assert(Functions.duplicateDistinct(List(1, 2), List(3)) === Queue(1, 1, 1))
    assert(Functions.duplicateDistinct(List(1, 2), List(3, 2, 1)) === Queue(1, 1, 1, 2, 2))
    assert(Functions.duplicateDistinct(List(1, 2, 3), List(0, 3, 1, 4)) === Queue(2,2,2,3))
    assert(Functions.duplicateDistinct(List(1, 2, 2, 3), List(0, 3, 2, 1, 4)) === Queue(2, 2, 2, 3))
    assert(Functions.duplicateDistinct(List(3, 3, 3), List(1, 2, 3)) === Queue(3))
    assert(Functions.duplicateDistinct(List(1, 2, 3), List(-1, -2, -3)) === Queue())
  }

  test("getDebugName") {

    val p = new Point(3, 4);
    assertResult("Point") {
      p.printDebugName()
      p.getDebugName
    }
  }

  test("getDebugVars") {
    val p = new Point(3, 4);
    assertResult(Map(
      "x" -> (3, false),
      "y" -> (4, false),
      "a" -> ("test", false)
    )) {
      p.printDebugVars()
      p.getDebugVars
    }
  }

}
