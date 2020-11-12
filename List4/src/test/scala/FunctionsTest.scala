import org.scalatest.FunSuite

class FunctionsTest extends FunSuite {
  test("tree.test") {
    val t = Functions.generateTree(2, 0, 25)
    print(t)
  }

  test("tree.diff") {
    val t1 = BTree(4, Vertex(2), Vertex(7, Vertex(5), Empty))
    val t2 = BTree(1, Vertex(-2), Vertex(2, Vertex(5), Empty))

    val tRes = Functions.elementwiseDiff(t1, t2)(Functions.intRootDiff)
    print(tRes)
  }
}
