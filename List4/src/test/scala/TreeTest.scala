import org.scalatest.FunSuite

class TreeTest extends FunSuite {
  test("tree.empty") {
    assert(BTree() === Empty)
  }

  test("tree.isEmpty") {
    assert(BTree().isEmpty)
  }

  test("tree.emptyRootOption") {
    assert(Empty.rootOption === None)
  }

  test("tree.emptyLeftRightOption") {
    assert(Empty.leftOption === None)
    assert(Empty.rightOption === None)
  }

  test("tree.singleRootOption") {
    assert(Vertex(5).rootOption === Some(5))
  }

  test("tree.singleLeftRightOption") {
    val t = Vertex(5)
    assert(t.leftOption === None)
    assert(t.rightOption === None)
  }

  test("tree.singleVertexRootOption") {
    assert(Vertex(5).rootOption === Some(5))
  }

  test("tree.leftSubtreeEmpty") {
    assert(Empty.leftOption === None)
  }

  test("tree.leftSubtreeSingleRoot") {
    assert(Vertex(5, Empty, Empty).leftOption === None)
  }

  test("tree.leftSubtreeNonempty") {
    val t = Vertex(1, Vertex(2), Vertex(3))
    assert(t.leftOption === Some(Vertex(2)))
  }

  test("tree.leftSubtreeFull") {
    val t = Vertex(1, Vertex(2, Vertex(8), Vertex(9)), Vertex(3))
    assert(t.leftOption.get === Vertex(2, Vertex(8), Vertex(9)))
  }
}
