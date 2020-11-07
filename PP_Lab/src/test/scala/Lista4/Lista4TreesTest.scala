package Lista4
import Lista4.Lista4Trees.{Empty, Node, inorder}
import org.scalatest.FunSuite

class Lista4TreesTest extends FunSuite{
  test("Lista 4 zadanie 1"){
    val tree1 = Lista4Trees.createTree(2, 1, 10)
    val tree2 = Lista4Trees.createTree(0, 1, 10)
    val tree3 = Lista4Trees.createTree(3, 1, 10)
    val tree4 = Lista4Trees.createTree(4, 1, 10)

    assert(Lista4Trees.isTreeFull(tree1))
    assert(Lista4Trees.isTreeFull(tree2))
    assert(Lista4Trees.isTreeFull(tree3))
    assert(Lista4Trees.isTreeFull(tree4))

    assert(Lista4Trees.isTreeDepthRight(tree1, 2))
    assert(Lista4Trees.isTreeDepthRight(tree2, 0))
    assert(Lista4Trees.isTreeDepthRight(tree3, 3))
    assert(Lista4Trees.isTreeDepthRight(tree4, 4))
  }

  test("Lista 4 zadanie 3 DFS"){
    val tree1 = Node(2, Node(3, Node(2, Empty, Empty), Node(4, Empty, Empty)), Node(3, Node(3, Empty, Empty), Node(2, Empty, Empty)))
    val tree2 =  Node(4, Node(2, Node(2, Empty, Empty), Node(4, Empty, Empty)), Node(3, Node(5, Empty, Empty), Node(5, Empty, Empty)))
    val result12 = Lista4Trees.compareTreesDFS(tree1, tree2)
    assert(result12._1 == Node(2, Node(3, Empty, Empty), Node(-1, Node(3, Empty, Empty), Node(2, Empty, Empty))))
    assert(result12._2 == Node(4, Node(2, Empty, Empty), Node(-1, Node(5, Empty, Empty), Node(5, Empty, Empty))))

    val tree3 = Node(8, Node(10, Node(1, Empty, Empty), Node(2, Empty, Empty)), Node(7, Node(3, Empty, Empty), Node(15, Empty, Empty)))
    val tree4 =  Node(8, Node(1, Node(1, Empty, Empty), Node(4, Empty, Empty)), Node(7, Node(3, Empty, Empty), Node(1, Empty, Empty)))
    val result34 = Lista4Trees.compareTreesDFS(tree3, tree4)
    assert(result34._1 == Node(-1, Node(10, Empty, Node(2, Empty, Empty)), Node(-1, Empty, Node(15, Empty, Empty))))
    assert(result34._2 == Node(-1, Node(1, Empty, Node(4, Empty, Empty)), Node(-1, Empty, Node(1, Empty, Empty))))
  }

  test("Lista 4 zadanie 3 BFS"){
    val tree1 = Node(2, Node(3, Node(2, Empty, Empty), Node(4, Empty, Empty)), Node(3, Node(3, Empty, Empty), Node(2, Empty, Empty)))
    val tree2 =  Node(4, Node(2, Node(2, Empty, Empty), Node(4, Empty, Empty)), Node(3, Node(5, Empty, Empty), Node(5, Empty, Empty)))
    val result12 = Lista4Trees.compareTreesBFS(tree1, tree2)
    assert(result12._1 == Node(2, Node(3, Empty, Empty), Node(-1, Node(3, Empty, Empty), Node(2, Empty, Empty))))
    assert(result12._2 == Node(4, Node(2, Empty, Empty), Node(-1, Node(5, Empty, Empty), Node(5, Empty, Empty))))

    val tree3 = Node(8, Node(10, Node(1, Empty, Empty), Node(2, Empty, Empty)), Node(7, Node(3, Empty, Empty), Node(15, Empty, Empty)))
    val tree4 =  Node(8, Node(1, Node(1, Empty, Empty), Node(4, Empty, Empty)), Node(7, Node(3, Empty, Empty), Node(1, Empty, Empty)))
    val result34 = Lista4Trees.compareTreesBFS(tree3, tree4)
    assert(result34._1 == Node(-1, Node(10, Empty, Node(2, Empty, Empty)), Node(-1, Empty, Node(15, Empty, Empty))))
    assert(result34._2 == Node(-1, Node(1, Empty, Node(4, Empty, Empty)), Node(-1, Empty, Node(1, Empty, Empty))))
  }

}
