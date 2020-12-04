import java.lang.reflect.AnnotatedType
import lab05.testClasses.{Dog, Point}
import org.scalatest.FunSuite

class DebugTest extends FunSuite {

  val point = new Point(3, 4)
  val dog = new Dog("Rex", 10)

  test("debugName & debugVars test") {
    point.debugName()
    point.debugVars()
    dog.debugName()
    dog.debugVars()
    dog.vaccinated = true;
    dog.debugVars()
  }

  test("debugStringName test") {
    assert(point.debugStringName == "Point")
    assert(dog.debugStringName == "Dog")
  }

  test("debugVarsArray test") {
    point.debugName()
    printArray(point.debugVarsArray)
    dog.debugName()
    printArray(dog.debugVarsArray)
  }

  def printArray(array: Array[(String, AnnotatedType, AnyRef)]): Unit = {
    for (elem <- array) {
      println("Var: " + elem._1 + "=>" + elem._2 + ", " + elem._3)
    }
  }

}
