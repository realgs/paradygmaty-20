import org.scalatest.FunSuite

class GetClassNameTest extends FunSuite {

  test("pointClassTest") {
    val point = new Point(3,4)
    assert(point.getClassName == "Point")
  }

  test("printTestClassTest") {
    val printTest = new PrintTest("name" ,1)
    assert(printTest.getClassName == "PrintTest")
  }

}
