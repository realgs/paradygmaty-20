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

  test("eachNElement.second") {
    val lxs = LazyList(1, 2, 3, 4, 5, 6, 7)
    assert(Functions.eachNElement(lxs, 2) === LazyList(1, 3, 5, 7))
  }

  test("eachNElement.third") {
    val lxs = LazyList(1, 2, 3, 4, 5, 6, 7)
    assert(Functions.eachNElement(lxs, 3) === LazyList(1, 4, 7))
  }

  test("eachNElement.forth") {
    val lxs = LazyList(1, 2, 3, 4, 5, 6, 7)
    assert(Functions.eachNElement(lxs, 4) === LazyList(1, 5))
  }

  test("apply.oneEmptyAddition") {
    Functions.apply(LazyList(), LazyList(1, 2, 3, 4))(_ + _) === LazyList(1, 2, 3, 4)
  }

  test("apply.simpleAddition") {
    Functions.apply(LazyList(1, 2), LazyList(3, 5))(_ + _) === LazyList(4, 7)
  }
}
