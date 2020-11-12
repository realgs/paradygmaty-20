import org.scalatest.FunSuite

class FunctionsTest extends FunSuite {
  test("tree.test") {
    val t = Functions.generateTree(4, 0, 25)
    t.printTree()
  }
}
