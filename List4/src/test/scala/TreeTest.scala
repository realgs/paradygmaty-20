import org.scalatest.FunSuite

class TreeTest extends FunSuite {
  test("tree.empty") {
    assert(BTree() === Empty)
  }

  test("tree.emptyRootOption") {
    assert(Empty.rootOption === None)
  }

  test("tree.singleVertexRootOption") {
    assert(Vertex(5).rootOption === Some(5))
  }

  test("tree.leftSubtreeEmpty") {
    assert(Empty.getLeft === None)
  }

  test("tree.leftSubtreeSingleRoot") {
    assert(Vertex(5, Empty, Empty).getLeft === Some(Empty))
  }

  test("tree.leftSubtreeNonempty") {
    val t = Vertex(1, Vertex(2), Vertex(3))
    assert(t.getLeft === Some(Vertex(2)))
  }

  test("tree.leftSubtreeFull") {
    val t = Vertex(1, Vertex(2, Vertex(8), Vertex(9)), Vertex(3))
    assert(t.getLeft.value === Vertex(2, Vertex(8), Vertex(9)))
  }
}
