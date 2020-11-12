import org.scalatest.FunSuite

class FunctionsTest extends FunSuite {
  test("tree.test") {
    val t = Functions.generateTree(2, 0, 25)
    print(t)
  }

  test("tree.diff") {
    val t1 = BTree(4, Vertex(2), Vertex(7, Vertex(5), Empty))
    val t2 = BTree(1, Vertex(-2), Vertex(3, Vertex(5), Empty))

    def elementwiseDiff(t1: BTree[Int], t2: BTree[Int])(rootDiff: (BTree[Int], BTree[Int]) => Option[Int]): BTree[Int] = {
      if (rootDiff(t1, t2).isEmpty) Empty
      else {
        BTree(rootDiff(t1, t2).get, elementwiseDiff(t1.getLeft.getOrElse(Empty), t2.getLeft.getOrElse(Empty))(rootDiff),
          elementwiseDiff(t1.getRight.getOrElse(Empty), t2.getRight.getOrElse(Empty))(rootDiff))
      }
    }

    val tRes = elementwiseDiff(t1, t2)(Functions.intRootDiff)
    print(tRes)
  }
}
