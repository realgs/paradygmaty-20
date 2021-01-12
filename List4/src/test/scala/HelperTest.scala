import org.scalatest.FunSuite

class HelperTest extends FunSuite{
  test("formTree.allNil") {
    val res = Helper.formTree(Nil, Nil)(Nil, Nil)(Nil, Nil)
    assert(res === (Nil, Nil))
  }

  test("formTree.emptyLower") {
    val res = Helper.formTree(List(3, 2, 7, 9), List(3, 2, 6, 9))(List.fill(8)(Empty), List.fill(8)(Empty))(Nil, Nil)
    
    assert(res === (List(Empty, Empty, Vertex(7,Empty,Empty), Empty),List(Empty, Empty, Vertex(6,Empty,Empty), Empty)))
  }

  test("formTree.nonEmptyLower") {
    val res = Helper.formTree(List(5, 4), List(7, 4))(List(Empty, Empty, Vertex(7), Empty),
      List(Empty, Empty, Vertex(6), Empty))(Nil, Nil)

    assert(res === (List(Vertex(5), Vertex(-1,Vertex(7),Empty)),List(Vertex(7), Vertex(-1,Vertex(6),Empty))))
  }

  test("isFull.emptyTree") {
    assert(Helper.isFull(Empty))
  }

  test("isFull.singleRoot") {
    assert(Helper.isFull(Vertex(5)))
  }

  test("isFull.onlyLeftNodes") {
    assert(!Helper.isFull(Vertex(5, Vertex(4, Vertex(3), Empty), Empty)))
  }

}
