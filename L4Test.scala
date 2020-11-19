package List4Test
import org.scalatest.FunSuite
import L4.L4
import _root_.L4.L4.Node
import _root_.L4.L4.Empty
import _root_.L4.L4.+
import _root_.L4.L4.-
import _root_.L4.L4.*
import _root_.L4.L4./

class L4Test extends FunSuite {
  test("Test of helper function 'depthBT'") {
    assert(L4.depthBT(Empty()) == 0)
    assert(L4.depthBT(Node(1, Empty(), Empty())) == 1)
    assert(L4.depthBT(Node(1, Node(2, Node(3, Empty(), Empty()), Empty()), Empty())) == 3)
    assert(L4.depthBT(Node(1, Node(2, Empty(), Empty()), Node(1, Node(2, Node(3, Empty(), Empty()), Empty()), Empty()))) == 4)
  }

  test("test of helper function 'numberOfNodesBT'") {
    assert(L4.numberOfNodesBT(Empty()) == 0)
    assert(L4.numberOfNodesBT(Node(1, Empty(), Empty())) == 1)
    assert(L4.numberOfNodesBT(Node(1, Node(2, Node(4, Empty(), Empty()), Node(5, Empty(), Empty())), Node(3, Node(4, Empty(), Empty()), Node(5, Empty(), Empty())))) == 7)
    assert(L4.numberOfNodesBT(Node(1, Node(2, Empty(), Empty()), Empty())) == 2)
    assert(L4.numberOfNodesBT(Node("a", Node("b", Node("c", Empty(), Empty()), Empty()), Node("d", Empty(), Node("e", Empty(), Empty())))) == 5)
  }

  test("test of helper function 'isFullBT'") {
    assert(L4.isFullBT(Empty()))
    assert(L4.isFullBT(Node(1, Empty(), Empty())))
    assert(L4.isFullBT(Node(1, Node(2, Node(4, Empty(), Empty()), Node(5, Empty(), Empty())), Node(3, Node(4, Empty(), Empty()), Node(5, Empty(), Empty())))))
    assert(!L4.isFullBT(Node(1, Node(2, Empty(), Empty()), Empty())))
    assert(!L4.isFullBT(Node("a", Node("b", Node("c", Empty(), Empty()), Empty()), Node("d", Empty(), Node("e", Empty(), Empty())))))
  }

  test("test of task 1 -> function 'generateBT'") {
    val tree1 = L4.generateBT(0, 1)
    assert(L4.isFullBT(tree1))
    assert(L4.depthBT(tree1) == 0)
    assert(L4.areInRange(L4.DFS_BT(tree1), 0, 1))
    val tree2 = L4.generateBT(5, 10)
    assert(L4.isFullBT(tree2))
    assert(L4.depthBT(tree2) == 5)
    assert(L4.areInRange(L4.DFS_BT(tree2), 0, 10))
    val tree3 = L4.generateBT(10, 1, 150)
    assert(L4.isFullBT(tree3))
    assert(L4.depthBT(tree3) == 10)
    assert(L4.areInRange(L4.DFS_BT(tree3), 1, 150))
    val tree4 = L4.generateBT(15, 8, 15)
    assert(L4.isFullBT(tree4))
    assert(L4.depthBT(tree4) == 15)
    assert(L4.areInRange(L4.DFS_BT(tree4), 8, 15))
    val tree5 = L4.generateBT(20, 80, 80)
    assert(L4.isFullBT(tree5))
    assert(L4.depthBT(tree5) == 20)
    assert(L4.areInRange(L4.DFS_BT(tree5), 80, 80))
  }

  test ("test od task 2 -> function 'differenceBT'") {
    val testTree1a = Node(1, Node(2, Empty(), Empty()), Node(3, Empty(), Empty()))
    val testTree1b = Node(1, Node(1, Empty(), Empty()), Node(1, Empty(), Empty()))
    assert(L4.differenceBT(testTree1a, testTree1b) == Node(0, Node(1, Empty(), Empty()), Node(2, Empty(), Empty())))
    val generatedTestTree = L4.generateBT(3, 5)
    assert(L4.differenceBT(generatedTestTree, generatedTestTree) == Node(0, Node(0, Node(0, Empty(), Empty()), Node(0, Empty(), Empty())), Node(0, Node(0, Empty(), Empty()), Node(0, Empty(), Empty()))))
    val testTree3a = Node(1, Node(2, Node(4, Empty(), Empty()), Node(5, Empty(), Empty())), Node(3, Node(6, Empty(), Empty()), Node(7, Empty(), Empty())))
    val testTree3b = Node(1, Node(-2, Node(45, Empty(), Empty()), Node(50, Empty(), Empty())), Node(31, Node(60, Empty(), Empty()), Node(8, Empty(), Empty())))
    assert(L4.differenceBT(testTree3a, testTree3b) == Node(0, Node(4, Node(-41, Empty(), Empty()), Node(-45, Empty(), Empty())), Node(-28, Node(-54, Empty(), Empty()), Node(-1, Empty(), Empty()))))
    val testTree4a = Node(1, Node(2, Node(4, Empty(), Empty()), Node(5, Empty(), Empty())), Node(3, Node(6, Empty(), Empty()), Node(7, Empty(), Empty())))
    val testTree4b = Node(0, Node(0, Node(0, Empty(), Empty()), Node(0, Empty(), Empty())), Node(0, Node(0, Empty(), Empty()), Node(0, Empty(), Empty())))
    assert(L4.differenceBT(testTree4a, testTree4b) == testTree4a)
  }

  test ("test od task 3 -> function 'removeDuplicatesBT_DFS'") {
    val testTree1 = Node(1, Node(2, Empty(), Empty()), Node(3, Empty(), Empty()))
    assert(L4.removeDuplicatesBT_DFS(testTree1, testTree1) == (Empty(), Empty()))
    val testTree2a = Node(1, Node(2, Empty(), Empty()), Node(3, Empty(), Empty()))
    val testTree2b = Node(1, Node(1, Empty(), Empty()), Node(1, Empty(), Empty()))
    assert(L4.removeDuplicatesBT_DFS(testTree2a, testTree2b) == (Node(-1, Node(2, Empty(), Empty()), Node(3, Empty(), Empty())), Node(-1, Node(1, Empty(), Empty()), Node(1, Empty(), Empty()))))
    val testTree3a = Node(1, Node(2, Node(4, Empty(), Empty()), Node(5, Empty(), Empty())), Node(3, Node(6, Empty(), Empty()), Node(7, Empty(), Empty())))
    val testTree3b = Node(1, Node(2, Node(10, Empty(), Empty()), Node(5, Empty(), Empty())), Node(3, Node(6, Empty(), Empty()), Node(7, Empty(), Empty())))
    assert(L4.removeDuplicatesBT_DFS(testTree3a, testTree3b) == (Node(-1, Node(-1, Node(4, Empty(), Empty()), Empty()), Empty()), Node(-1, Node(-1, Node(10, Empty(), Empty()), Empty()), Empty())))
    val testTree4a = Node(1, Node(2, Node(4, Empty(), Empty()), Node(5, Empty(), Empty())), Node(3, Node(6, Empty(), Empty()), Node(7, Empty(), Empty())))
    val testTree4b = Node(2, Node(2, Node(4, Empty(), Empty()), Node(5, Empty(), Empty())), Node(3, Node(8, Empty(), Empty()), Node(9, Empty(), Empty())))
    assert(L4.removeDuplicatesBT_DFS(testTree4a, testTree4b) == (Node(1, Empty(), Node(-1, Node(6, Empty(), Empty()), Node(7, Empty(), Empty()))), Node(2, Empty(), Node(-1, Node(8, Empty(), Empty()), Node(9, Empty(), Empty())))))
  }

  test ("test od task 3 -> function 'removeDuplicatesBT_BFS'") {
    val testTree1 = Node(1, Node(2, Empty(), Empty()), Node(3, Empty(), Empty()))
    assert(L4.removeDuplicatesBT_BFS(testTree1, testTree1) == (Empty(), Empty()))
    val testTree2a = Node(1, Node(2, Empty(), Empty()), Node(3, Empty(), Empty()))
    val testTree2b = Node(1, Node(1, Empty(), Empty()), Node(1, Empty(), Empty()))
    assert(L4.removeDuplicatesBT_BFS(testTree2a, testTree2b) == (Node(-1, Node(2, Empty(), Empty()), Node(3, Empty(), Empty())), Node(-1, Node(1, Empty(), Empty()), Node(1, Empty(), Empty()))))
    val testTree3a = Node(1, Node(2, Node(4, Empty(), Empty()), Node(5, Empty(), Empty())), Node(3, Node(6, Empty(), Empty()), Node(7, Empty(), Empty())))
    val testTree3b = Node(1, Node(2, Node(10, Empty(), Empty()), Node(5, Empty(), Empty())), Node(3, Node(6, Empty(), Empty()), Node(7, Empty(), Empty())))
    assert(L4.removeDuplicatesBT_BFS(testTree3a, testTree3b) == (Node(-1, Node(-1, Node(4, Empty(), Empty()), Empty()), Empty()), Node(-1, Node(-1, Node(10, Empty(), Empty()), Empty()), Empty())))
    val testTree4a = Node(1, Node(2, Node(4, Empty(), Empty()), Node(5, Empty(), Empty())), Node(3, Node(6, Empty(), Empty()), Node(7, Empty(), Empty())))
    val testTree4b = Node(2, Node(2, Node(4, Empty(), Empty()), Node(5, Empty(), Empty())), Node(3, Node(8, Empty(), Empty()), Node(9, Empty(), Empty())))
    assert(L4.removeDuplicatesBT_BFS(testTree4a, testTree4b) == (Node(1, Empty(), Node(-1, Node(6, Empty(), Empty()), Node(7, Empty(), Empty()))), Node(2, Empty(), Node(-1, Node(8, Empty(), Empty()), Node(9, Empty(), Empty())))))
  }

  test ("test of task 4 -> function 'eachNElement'") {
    assert(L4.eachNElement(LazyList(5,6,3,2,1), 2, 3) == LazyList(5,3))
    assert(L4.eachNElement(LazyList(5,6,3,2,1), 2, 4) == LazyList(5,3))
    assert(L4.eachNElement(LazyList(5,6,3,2,1), 2, 5) == LazyList(5,3,1))
    assert(L4.eachNElement(LazyList(1,2,3,4,5,6,7,8,9), 1, 5) == LazyList(1,2,3,4,5))
    assert(L4.eachNElement(LazyList(1,2,3,4,5,6,7,8,9), 3, 8) == LazyList(1,4,7))
    assert(L4.eachNElement(LazyList('a', 'b', 'c', 'd', 'e'), 2, 5) == LazyList('a', 'c', 'e'))
  }

  test("test of task 5 -> function 'lOperation'") {
    assert(L4.lOperation(LazyList(1,2,3,4,5), LazyList(1,2,3,4,5), +) == LazyList(2,4,6,8,10))
    assert(L4.lOperation(LazyList(1,2,3,4,5), LazyList(1,2,3,4,5), -) == LazyList(0,0,0,0,0))
    assert(L4.lOperation(LazyList(1,2,3,4,5), LazyList(1,2,3,4,5), *) == LazyList(1,4,9,16,25))
    assert(L4.lOperation(LazyList(1,2,3,4,5), LazyList(1,2,3,4,5), /) == LazyList(1,1,1,1,1))
    assert(L4.lOperation(LazyList(5,10,15), LazyList(2,4,6), +) == LazyList(7,14,21))
    assert(L4.lOperation(LazyList(5,10,15), LazyList(2,4,6), -) == LazyList(3,6,9))
    assert(L4.lOperation(LazyList(5,10,15), LazyList(2,4,6), *) == LazyList(10,40,90))
    assert(L4.lOperation(LazyList(5,10,15), LazyList(2,4,6), /) == LazyList(2,2,2))
  }
}

