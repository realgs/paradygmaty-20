import org.scalatest.FunSuite

class TreeTest extends FunSuite {
  test("tree.empty") {
    assert(BTree() === Empty)
  }

  test("tree.isEmpty") {
    assert(BTree().isEmpty)
  }

  test("tree.isEmptyFullTree") {
    assert(!Vertex(5, Vertex(2), Vertex(1)).isEmpty)
  }

  test("tree.emptyIsLeaf") {
    assert(!Empty.isLeaf)
  }

  test("tree.singleVertexIsLeaf") {
    assert(Vertex(9).isLeaf)
  }

  test("tree.fullTreeIsLeaf") {
    val t = Vertex(1, Vertex(2), Vertex(3))
    assert(!t.isLeaf)
  }

  test("tree.emptyRootOption") {
    assert(Empty.rootOption === None)
  }

  test("tree.emptyLeftRightOption") {
    assert(Empty.leftOption === None)
    assert(Empty.rightOption === None)
  }

  test("tree.singleVertexRootOption") {
    assert(Vertex(5).rootOption === Some(5))
  }

  test("tree.singleLeftRightOption") {
    val t = Vertex(5)
    assert(t.leftOption === None)
    assert(t.rightOption === None)
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

  test("tree.fullTreeLeftRightOption") {
    val t = Vertex(5, Vertex(1), Vertex(7))
    assert(t.leftOption === Some(Vertex(1)))
    assert(t.rightOption === Some(Vertex(7)))
  }

  test("tree.leftSubtreeFull") {
    val t = Vertex(1, Vertex(2, Vertex(8), Vertex(9)), Vertex(3))
    assert(t.leftOption === Some(Vertex(2, Vertex(8), Vertex(9))))
  }

  test("tree.leftRightRootEmpty") {
    assert(Empty.leftRoot === None)
    assert(Empty.rightRoot === None)
  }

  test("tree.leftRightRootSingleVertex") {
    assert(Vertex(5).leftRoot === None)
    assert(Vertex(5).rightRoot === None)
  }

  test("tree.leftRightRootFullTree") {
    val t = Vertex(5, Vertex(2), Vertex(1))
    assert(t.leftRoot === Some(2))
    assert(t.rightRoot === Some(1))
  }

  test("toBFSList.empty") {
    assert(Empty.toBfsList === List())
  }

  test("toBFSList.singleVertex") {
    assert(Vertex(5).toBfsList === List(5))
  }

  test("toBFSList.fullTree") {
    val t = BTree(4, Vertex(2, Vertex(7), Vertex(13)), Vertex(7, Vertex(5), Vertex(8)))
    assert(t.toBfsList === List(4, 2, 7, 7, 13, 5, 8))
  }

  test("toBFSList.notFullTree") {
    val t = BTree(1, Vertex(2, Vertex(8), Vertex(9)), Vertex(3))
    assert(t.toBfsList === List(1, 2, 3, 8, 9))
  }
}
