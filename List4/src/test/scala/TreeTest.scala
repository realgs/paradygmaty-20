import org.scalatest.FunSuite

class TreeTest extends FunSuite {
  test("tree.empty") {
    assert(BTree() === Empty)
  }

  test("tree.leftSubtreeEmpty") {
    assert(Empty.getLeft === None)
  }

  test("tree.leftSubtreeSingleRoot") {
    assert(Vertex(5, Empty, Empty).getLeft.value === Empty)
  }

  test("tree.leftSubtreeNonempty") {
    val t = Vertex(1, Vertex(2), Vertex(3))
    assert(t.getLeft.value === Vertex(2))
  }
}
