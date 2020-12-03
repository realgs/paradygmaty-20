import org.scalatest.FunSuite

class GetClassFieldsTest extends FunSuite {

  test("pointClassTest") {
    val point = new Point(3,4)
    assert(point.getClassFields == List(("x","int",3), ("y","int",4),("a","java.lang.String","test")))
  }

  test("printTestClassTest") {
    val printTest = new PrintTest("test",2)
    assert(printTest.getClassFields == List(("nameNumber","java.lang.String","test2"), ("doubles","scala.collection.immutable.List<java.lang.Object>",List(1.0, 5.5, 9.0))))
  }
}
