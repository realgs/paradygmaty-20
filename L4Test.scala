import org.scalatest.FunSuite
import List4.L4Trees
import List4.L4LazyLists
import org.scalatest.Matchers.convertToAnyShouldWrapper
import org.scalatest.Matchers.be

class L4Test extends FunSuite {

  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

  test("Test for 'createTree' and 'substractionNode' functions:") {
    val t = L4Trees.createTree(4, 0, 20)
    val t2 = L4Trees.createTree(4, 0, 20)
    //Sprawdzenie czy otrzymane drzewa są pełne
    assert(L4Trees.ifFullTree(t))
    assert(L4Trees.ifFullTree(t2))
    assert(L4Trees.checkDepth(t2, 0) == 4)
    println(L4Trees.breadthBT(t))
    println(L4Trees.breadthBT(t2))
    val t3 = L4Trees.substractionNode(t,t2)
    println(L4Trees.breadthBT(t3))
  }

  test("Test for 'repeatingNodesDepth' function:") {
    //assert(L4Trees.repeatingNodesDepth(L4Trees.t3, L4Trees.t4) == (Node(-1, Empty, Node(-1, Node(10, Empty, Empty), Node(-1, Node(14, Empty, Empty), Node(15, Empty, Empty)))), Node(-1, Empty, Node(-1, Node(9, Empty, Empty), Node(-1, Node(13, Empty, Empty), Node(17, Empty, Empty))))))
    //assert(L4Trees.repeatingNodesDepth(L4Trees.t, L4Trees.t2) == (Node(-1, Node(2, Empty, Empty), Empty), Node(-1, Node(4, Empty, Empty), Empty)))
    //val (tree1: BT[Int], tree2:BT[Int]) = (Node(-1,Empty,Node(-1,Node(10,Empty,Empty),Node(-1,Node(14,Empty,Empty),Node(15,Empty,Empty)))),Node(-1,Empty,Node(-1,Node(9,Empty,Empty),Node(-1,Node(13,Empty,Empty),Node(17,Empty,Empty)))))
    val tree1: BT[Int] = Node(-1,Empty,Node(-1,Node(10,Empty,Empty),Node(-1,Node(14,Empty,Empty),Node(15,Empty,Empty))))
    val tree3 = L4Trees.repeatingNodesDepth(L4Trees.t3, L4Trees.t4)
    println(tree1 == tree3._1)
    //L4Trees.repeatingNodesDepth(L4Trees.t3, L4Trees.t4) shouldEqual ((Node(-1,Empty,Node(-1,Node(10,Empty,Empty),Node(-1,Node(14,Empty,Empty),Node(15,Empty,Empty)))),Node(-1,Empty,Node(-1,Node(9,Empty,Empty),Node(-1,Node(13,Empty,Empty),Node(17,Empty,Empty))))))
    //L4Trees.repeatingNodesDepth(L4Trees.t3, L4Trees.t4) should be((Node(-1,Empty,Node(-1,Node(10,Empty,Empty),Node(-1,Node(14,Empty,Empty),Node(15,Empty,Empty)))),Node(-1,Empty,Node(-1,Node(9,Empty,Empty),Node(-1,Node(13,Empty,Empty),Node(17,Empty,Empty))))))
    //assert(L4Trees.repeatingNodesDepth(L4Trees.t3, L4Trees.t4) == (Node(-1,Empty,Node(-1,Node(10,Empty,Empty),Node(-1,Node(14,Empty,Empty),Node(15,Empty,Empty)))),Node(-1,Empty,Node(-1,Node(9,Empty,Empty),Node(-1,Node(13,Empty,Empty),Node(17,Empty,Empty))))))
    //(Node(-1,Empty,Node(-1,Node(10,Empty,Empty),Node(-1,Node(14,Empty,Empty),Node(15,Empty,Empty)))),Node(-1,Empty,Node(-1,Node(9,Empty,Empty),Node(-1,Node(13,Empty,Empty),Node(17,Empty,Empty))))) == (Node(-1,Empty,Node(-1,Node(10,Empty,Empty),Node(-1,Node(14,Empty,Empty),Node(15,Empty,Empty)))),Node(-1,Empty,Node(-1,Node(9,Empty,Empty),Node(-1,Node(13,Empty,Empty),Node(17,Empty,Empty)))))
    //assert(L4Trees.repeatingNodesDepth(L4Trees.t5, L4Trees.t6) == (Node(1, Node(1, Node(-5, Empty, Empty), Node(-27, Empty, Empty)), Node(6, Node(18, Empty, Empty), Node(1, Empty, Empty))), Node(8, Node(9, Node(6, Empty, Empty), Node(27, Empty, Empty)), Node(10, Node(19, Empty, Empty), Node(0, Empty, Empty)))))
    //assert(L4Trees.repeatingNodesDepth(L4Trees.t7, L4Trees.t7) == (Empty, Empty))
    //assert(L4Trees.repeatingNodesDepth(L4Trees.t7, L4Trees.t8) == (Node(1, Empty, Empty), Node(8, Empty, Empty)))
    assertThrows[Exception](L4Trees.repeatingNodesDepth(L4Trees.t5, L4Trees.t4))
  }

  test("Test for 'repeatingNodesBreadth' function:") {
    //assert(L4Trees.repeatingNodesBreadth(L4Trees.t, L4Trees.t2) == (Node(-1, Node(2, Empty, Empty), Empty), Node(-1, Node(4, Empty, Empty), Empty)))
    //assert(L4Trees.repeatingNodesBreadth(L4Trees.t3, L4Trees.t4) == (Node(-1,Empty,Node(-1,Node(10,Empty,Empty),Node(-1,Node(14,Empty,Empty),Node(15,Empty,Empty)))),Node(-1,Empty,Node(-1,Node(9,Empty,Empty),Node(-1,Node(13,Empty,Empty),Node(17,Empty,Empty))))))
    //assert(L4Trees.repeatingNodesBreadth(L4Trees.t5, L4Trees.t6) == (Node(1, Node(1, Node(-5, Empty, Empty), Node(-27, Empty, Empty)), Node(6, Node(18, Empty, Empty), Node(1, Empty, Empty))), Node(8, Node(9, Node(6, Empty, Empty), Node(27, Empty, Empty)), Node(10, Node(19, Empty, Empty), Node(0, Empty, Empty)))))
    //assert(L4Trees.repeatingNodesBreadth(L4Trees.t7, L4Trees.t7) == (Empty, Empty))
    //assert(L4Trees.repeatingNodesBreadth(L4Trees.t7, L4Trees.t8) == (Node(1, Empty, Empty), Node(8, Empty, Empty)))
    assertThrows[Exception](L4Trees.repeatingNodesBreadth(L4Trees.t5, L4Trees.t4))
  }

  test("Test for 'eachNElement' function:") {
    assert(L4LazyLists.eachNElement(LazyList(5,6,3,2,1), 2, 3) == LazyList(5,3))
    assert(L4LazyLists.eachNElement(LazyList(5,6,3,2,1), 2, 4) == LazyList(5,3))
    assert(L4LazyLists.eachNElement(LazyList(4,5,8,10,-1,8,0,1), 2, 7) == LazyList(4, 8, -1, 0))
    assert(L4LazyLists.eachNElement(LazyList(8.9,0.9,-3.0,-6,7), 1, 3) == LazyList(8.9,0.9,-3.0))
    assert(L4LazyLists.eachNElement(LazyList(), 9, 3) == LazyList())
    assert(L4LazyLists.eachNElement(LazyList(4,5,6), 1, 0) == LazyList(4))
    assertThrows[Exception](L4LazyLists.eachNElement(LazyList(9, 0, -2), -7, 0))
    assertThrows[Exception](L4LazyLists.eachNElement(LazyList(9, 0, -2), 1, -2))
    assert(L4LazyLists.eachNElement(LazyList('a','b','c','d','e','f','g','h','i','j'), 3, 8) == LazyList('a','d','g'))
    assert(L4LazyLists.eachNElement(LazyList('a','b','c','d','e','f','g','h','i','j'), 3, 9) == LazyList('a','d','g'))
    assert(L4LazyLists.eachNElement(LazyList('a','b','c','d','e','f','g','h','i','j'), 3, 10) == LazyList('a','d','g','j'))
  }

  test("Test for 'loperation' function:") {
    assert(L4LazyLists.loperation(LazyList(1,2,3), LazyList(2,3,4,5,9,-8))(L4LazyLists.+) == LazyList(3,5,7,5,9,-8))
    assert(L4LazyLists.loperation(LazyList(1,2,3), LazyList(2,0,4,5))(L4LazyLists.*) == LazyList(2,0,12,5))
    assert(L4LazyLists.loperation(LazyList(1,2,3), LazyList(2,0,4,5))(L4LazyLists.-) == LazyList(-1,2,-1,5))
    assert(L4LazyLists.loperation(LazyList(), LazyList(2,3,4,5))(L4LazyLists.-) == LazyList(2,3,4,5))
    assert(L4LazyLists.loperation(LazyList(1,2,3), LazyList())((number1, number2) => number1 + number2) == LazyList(1,2,3))
    assert(L4LazyLists.loperation(LazyList(), LazyList())(L4LazyLists.*) == LazyList())
    assert(L4LazyLists.loperation(LazyList(0,9,-8,5,78), LazyList(1,1,-1))(L4LazyLists./) == LazyList(0,9,8,5,78))
    assertThrows[Exception](L4LazyLists.loperation(LazyList(1,2,3), LazyList(2,1,0,5))(L4LazyLists./) == LazyList(0.5, 2))
  }
}