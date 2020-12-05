import org.scalatest.funsuite.AnyFunSuite

class DebugTest extends AnyFunSuite {

  val p = new Point(3, 4)

  //Zadanie 5
  test("Test debugNameStr; Debug Trait") {
    assert(p.debugNameStr() == "Point")
  }

  test("Test debugNameMap keys; Debug Trait") {
    val map = p.debugVarsMap()
    assert(map.contains("x"))
    assert(map.contains("y"))
    assert(map.contains("a"))
    assert(map.size == 3)
  }

  test("Test debugNameMap values; Debug Trait") {
    val map = p.debugVarsMap()
    assert(map.get("x") == Some(classOf[Int], 3))
    assert(map.get("y") == Some(classOf[Int], 4))
    assert(map.get("a") == Some(classOf[String], "test"))
    assert(map.size == 3)
  }


}
