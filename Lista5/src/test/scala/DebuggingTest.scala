import Debugging.Point
import org.scalatest.FunSuite

class DebuggingTest extends FunSuite {

  //Zad 3 test
  test("debugName.point") {
    val p : Point = new Point(3,4);
    p.debugName();
  }

}
