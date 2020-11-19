import org.scalatest.FunSuite

class FunctionsTest extends FunSuite {
  // Task 1
  test("generateTree.emptyTreeDepth") {
    val t = Functions.generateTree(-1, 1, 8)
    assert(t === Empty)
  }

  test("generateTree.zeroDepthTree") {
    val t = Functions.generateTree(0, 1, 2)
    assert(t.depth === 0)
    assert(t.isLeaf)
  }

  test("generateTree.testDepth") {
    val t = Functions.generateTree(2, 1, 100)
    assert(t.depth == 2)
  }

  // Task 2
  test("elementwiseDiff.bothEmpty") {
    val actual = Functions.elementwiseDiff(Empty, Empty)(Functions.intRootDiff)

    assert(actual === Empty)
  }

  test("elementwiseDiff.fullTree") {
    val t1 = BTree(4, Vertex(2, Vertex(7), Vertex(13)), Vertex(7, Vertex(5), Vertex(8)))
    val t2 = BTree(1, Vertex(-2, Vertex(1), Vertex(9)), Vertex(2, Vertex(5), Vertex(3)))

    val actual = Functions.elementwiseDiff(t1, t2)(Functions.intRootDiff)
    val expected = BTree(3, Vertex(4, Vertex(6), Vertex(4)), Vertex(5, Vertex(0), Vertex(5)))

    assert(actual === expected)
  }

  test("elementwiseDiff.notFullTree") {
    val t1 = BTree(4, Vertex(2), Vertex(7, Vertex(5), Empty))
    val t2 = BTree(1, Vertex(-2), Vertex(2, Vertex(5), Empty))

    val actual = Functions.elementwiseDiff(t1, t2)(Functions.intRootDiff)

    assert(actual === BTree(3, Vertex(4), Vertex(5, Vertex(0), Empty)))
  }

  test("elementwiseDiff.oneEmptyOneFull") {
    val t = Functions.generateTree(1, 0, 10)
    assert(Functions.elementwiseDiff(Empty, Empty)(Functions.intRootDiff) === Empty)
  }

  test("diff.differentDepths") {
    val (t1, t2) = (BTree(5), BTree(3, Vertex(2), Vertex(1)))
    val actual = Functions.elementwiseDiff(t1, t2)(Functions.intRootDiff)

    assert(actual === Vertex(2))
  }

  // Task 3
  test("deleteDuplicatesBFS.singleNodeIdentical") {
    val (t1, t2) = (Vertex(5), Vertex(5))
    val res = Functions.deleteDuplicatesDFS(t1, t2)

    assert(res === (Empty, Empty))
  }

  test("deleteDuplicatesBFS.singleNodeDifferent") {
    val (t1, t2) = (Vertex(3), Vertex(5))
    val res = Functions.deleteDuplicatesDFS(t1, t2)

    assert(res === (Vertex(3), Vertex(5)))
  }

  test("deleteDuplicatesDFS.threeNodesOneIdentical") {
    val t1 = BTree(1, Vertex(2), Vertex(3))
    val t2 = BTree(1, Vertex(5), Vertex(3))

    val res = Functions.deleteDuplicatesDFS(t1, t2)

    assert(res === (BTree(-1, Vertex(2), Empty), BTree(-1, Vertex(5), Empty)))
  }

  test("deleteDuplicatesDFS.notFullThreeLevel") {
    val t1 = BTree(1, Vertex(2, Vertex(8), Vertex(9)), Vertex(3))
    val t2 = BTree(1, Vertex(2, Vertex(7), Vertex(9)), Vertex(3))

    val (res1, res2) = Functions.deleteDuplicatesDFS(t1, t2)

    assert(res1 === BTree(-1, Vertex(-1, Vertex(8), Empty), Empty)
      && res2 === BTree(-1, Vertex(-1, Vertex(7), Empty), Empty))
  }

  test("deleteDuplicatesDFS.fullTreeLevel") {
    val t1 = BTree(1, Vertex(5, Vertex(3), Vertex(2)), Vertex(4, Vertex(7), Vertex(9)))
    val t2 = BTree(1, Vertex(7, Vertex(3), Vertex(2)), Vertex(4, Vertex(6), Vertex(9)))

    val expected1 = BTree(-1, Vertex(5), Vertex(-1, Vertex(7), Empty))
    val expected2 = BTree(-1, Vertex(7), Vertex(-1, Vertex(6), Empty))

    val (res1, res2) = Functions.deleteDuplicatesDFS(t1, t2)

    assert(res1 === expected1, res2 === expected2)
  }

  test("deleteDuplicatesBFS.threeNodesOneIdentical") {
    val t1 = BTree(1, Vertex(2), Vertex(3))
    val t2 = BTree(1, Vertex(5), Vertex(3))

    val (res1, res2) = Functions.deleteDuplicatesBFS(t1, t2)

    assert(res1 === BTree(-1, Vertex(2), Empty))
    assert(res2 === BTree(-1, Vertex(5), Empty))
  }

  test("deleteDuplicatesBFS.fullTreeLevel") {
    val t1 = BTree(1, Vertex(5, Vertex(3), Vertex(2)), Vertex(4, Vertex(7), Vertex(9)))
    val t2 = BTree(1, Vertex(7, Vertex(3), Vertex(2)), Vertex(4, Vertex(6), Vertex(9)))

    val expected1 = BTree(-1, Vertex(5), Vertex(-1, Vertex(7), Empty))
    val expected2 = BTree(-1, Vertex(7), Vertex(-1, Vertex(6), Empty))

    val (res1, res2) = Functions.deleteDuplicatesBFS(t1, t2)

    println(res1)
    println(res2)
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

  test("deleteDuplicatesDFS.speedTest*") {
    val DEPTH = 10
    val t1 = Functions.generateTree(DEPTH, 1, 14)
    val t2 = Functions.generateTree(DEPTH, 1, 14)

    time { Functions.deleteDuplicatesDFS(t1, t2) }
  }

  test("deleteDuplicatesBFS.speedTest*") {
    val DEPTH = 20
    val t1 = Functions.generateTree(DEPTH, 1, 100)
    val t2 = Functions.generateTree(DEPTH, 1, 100)

    time { Functions.deleteDuplicatesBFS(t1, t2) }
  }

  test("deleteDuplicates.randomCompare*") {
    val t1 = Functions.generateTree(10, 1, 14)
    val t2 = Functions.generateTree(10, 1, 14)

    val expected = Functions.deleteDuplicatesDFS(t1, t2)
    val actual = Functions.deleteDuplicatesBFS(t1, t2)

    assert(actual === expected)
  }

  // Task 4
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

  // Task5
  test("apply.oneEmptyAddition") {
    Functions.apply(LazyList(), LazyList(1, 2, 3, 4))(_ + _) === LazyList(1, 2, 3, 4)
  }

  test("apply.simpleAddition") {
    Functions.apply(LazyList(1, 2), LazyList(3, 5))(_ + _) === LazyList(4, 7)
  }
}
