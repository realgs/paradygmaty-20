import org.scalatest.FunSuite

class FunctionsTest extends FunSuite {
  test("tree.test") {
    val t = Functions.generateTree(2, 0, 25)
    print(t)
  }

  test("tree.diff") {
    val t1 = BTree(4, Vertex(2), Vertex(7, Vertex(5), Empty))
    val t2 = BTree(1, Vertex(-2), Vertex(2, Vertex(5), Empty))

    val res = Functions.elementwiseDiff(t1, t2)(Functions.intRootDiff)
    print(res)

    assert(res === BTree(3, Vertex(4), Vertex(5, Vertex(0), Empty)))
  }

  test("tree.lazyTraversal1") {
    val t1 = BTree(4, Vertex(2), Vertex(7, Vertex(5), Empty))
    val t2 = BTree(1, Vertex(-2), Vertex(2, Vertex(5), Empty))
    val trav1 = Functions.bottomUpDFS(t1)
    val trav2 = Functions.bottomUpDFS(t2)

    println(trav1.force)
    println(trav2.force)
  }

  test("tree.lazyTraversal2") {
    val t = BTree(1, Vertex(2, Vertex(8), Vertex(9)), Vertex(3))
    val actual = Functions.bottomUpDFS(t)

    println(actual.force)
  }

  test("toBFSList.test") {
    val t = BTree(1, Vertex(2, Vertex(8), Vertex(9)), Vertex(3))
    assert(t.toBfsList === List(1, 2, 3, 8, 9))
  }

  test("deleteDuplicates.singleNodeIdentical") {
    val (t1, t2) = (Vertex(5), Vertex(5))
    val res = Functions.deleteDuplicates(t1, t2)

    assert(res === (Empty, Empty))
  }

  test("deleteDuplicates.singleNodeDifferent") {
    val (t1, t2) = (Vertex(3), Vertex(5))
    val res = Functions.deleteDuplicates(t1, t2)

    assert(res === (Vertex(3), Vertex(5)))
  }

  test("deleteDuplicates.threeNodesOneIdentical") {
    val t1 = BTree(1, Vertex(2), Vertex(3))
    val t2 = BTree(1, Vertex(5), Vertex(3))

    val res = Functions.deleteDuplicates(t1, t2)

    assert(res === (BTree(-1, Vertex(2), Empty), BTree(-1, Vertex(5), Empty)))
  }

  test("deleteDuplicates.notFullThreeLevel") {
    val t1 = BTree(1, Vertex(2, Vertex(8), Vertex(9)), Vertex(3))
    val t2 = BTree(1, Vertex(2, Vertex(7), Vertex(9)), Vertex(3))

    val (res1, res2) = Functions.deleteDuplicates(t1, t2)

    assert(res1 === BTree(-1, Vertex(-1, Vertex(8), Empty), Empty)
      && res2 === BTree(-1, Vertex(-1, Vertex(7), Empty), Empty))
  }

  test("deleteDuplicates.fullTreeLevel") {
    val t1 = BTree(1, Vertex(5, Vertex(3), Vertex(2)), Vertex(4, Vertex(7), Vertex(9)))
    val t2 = BTree(1, Vertex(5, Vertex(3), Vertex(2)), Vertex(4, Vertex(6), Vertex(9)))

    val expected1 = BTree(-1, Empty, Vertex(-1, Vertex(7), Empty))
    val expected2 = BTree(-1, Empty, Vertex(-1, Vertex(6), Empty))

    val (res1, res2) = Functions.deleteDuplicates(t1, t2)

    assert(res1 === expected1, res2 === expected2)
  }

  test("eachNElement.second") {
    val lxs = LazyList(1, 2, 3, 4, 5, 6, 7)
    assert(Functions.eachNElement(lxs, 2) === LazyList(1, 3, 5, 7))
  }

  test("eachNElement.third") {
    val lxs = LazyList(1, 2, 3, 4, 5, 6, 7)
    assert(Functions.eachNElement(lxs, 3) === LazyList(1, 4, 7))
  }

  test("eachNElement.forth") {
    val lxs = LazyList(1, 2, 3, 4, 5, 6, 7)
    assert(Functions.eachNElement(lxs, 4) === LazyList(1, 5))
  }

  test("apply.oneEmptyAddition") {
    Functions.apply(LazyList(), LazyList(1, 2, 3, 4))(_ + _) === LazyList(1, 2, 3, 4)
  }

  test("apply.simpleAddition") {
    Functions.apply(LazyList(1, 2), LazyList(3, 5))(_ + _) === LazyList(4, 7)
  }
}
