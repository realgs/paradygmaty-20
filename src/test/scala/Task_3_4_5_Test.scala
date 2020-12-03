import org.scalatest.funsuite.AnyFunSuite

class Task_3_4_5_Test extends AnyFunSuite {
  test("task3.test") {
    val p: Point = new Point(3, 4)
    p.debugName()
  }

  test("task4.test") {
    val p: Point = new Point(3, 4)
    p.debugVars()
  }

  test("task5.test") {
    var p: Point = new Point(3, 4)
    assert(p.getNameOfClass() == "Class: Point")
    assert(p.getListWithVarsInfo() == List("Var: x => int,3", "Var: y => int,4", "Var: a => java.lang.String,test"))
  }
}
