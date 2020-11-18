import org.scalatest.FunSuite

class HelperTest extends FunSuite{
  test("formTree.test") {
    val res = Helper.formTree(List(3, 2, 7, 9), List(3, 2, 6, 9))(List.fill(8)(Empty), List.fill(8)(Empty))(Nil, Nil)

    assert(res === (List(Empty, Empty, Vertex(7,Empty,Empty), Empty),List(Empty, Empty, Vertex(6,Empty,Empty), Empty)))
  }

  test("formTree.test2") {
    val res = Helper.formTree(List(5, 4), List(7, 4))(List(Empty, Empty, Vertex(7), Empty), List(Empty, Empty, Vertex(6), Empty))(Nil, Nil)

    assert(res === (List(Vertex(5), Vertex(-1,Vertex(7),Empty)),List(Vertex(7), Vertex(-1,Vertex(6),Empty))))
  }
}
