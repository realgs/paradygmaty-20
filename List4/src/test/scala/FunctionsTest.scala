import org.scalatest.FunSuite

class FunctionsTest extends FunSuite {
  // Task 1
  test("generateTree.emptyTreeDepth") {
    val t = Functions.generateTree(-1, 1, 8)
    assert(t === Empty)
    assert(Helper.isFull(t))
  }

  test("generateTree.negativeDepth") {
    val t = Functions.generateTree(-5, 1, 100)
    assert(t === Empty)
    assert(Helper.isFull(t))
  }

  test("generateTree.zeroDepthTree") {
    val t = Functions.generateTree(0, 1, 2)
    assert(t.depth === 0)
    assert(Helper.isFull(t))
    assert(t.isLeaf)
  }

  test("generateTree.positiveDepth") {
    val t = Functions.generateTree(2, 1, 100)
    assert(t.depth == 2)
    assert(Helper.isFull(t))
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

  test("deleteDuplicatesDFS.oneFullOneEmpty") {
    val t1 = Empty
    val t2 = BTree(1, Vertex(7, Vertex(3), Vertex(2)), Vertex(4, Vertex(6), Vertex(9)))

    assertThrows[Exception] {
      Functions.deleteDuplicatesDFS(t1, t2)
    }
  }

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

    assert(res1 === expected1)
    assert(res2 === expected2)
  }

  // Task 4
  test("eachNElement.empty") {
    val lxs = LazyList()
    assert(Functions.eachNElement(lxs, 2, 5) === LazyList())
  }

  test("eachNElement.illegalStep") {
    assertThrows[IllegalArgumentException] {
      Functions.eachNElement(LazyList(1, 2, 3), 0, 5)
    }
  }

  test("eachNElement.illegalEndIndex") {
    assertThrows[IllegalArgumentException] {
      Functions.eachNElement(LazyList(1, 2, 3, 7, 5), 2, -2)
    }
  }

  test("eachNElement.example1") {
    val lxs = LazyList(5, 6, 3, 2, 1)
    assert(Functions.eachNElement(lxs, 2, 3) === LazyList(5, 3))
  }

  test("eachNElement.example2") {
    val lxs = LazyList(5, 6, 3, 2, 1)
    assert(Functions.eachNElement(lxs, 2, 4) === LazyList(5, 3))
  }

  test("eachNElement.example3") {
    val lxs = LazyList(5, 6, 3, 2, 1)
    assert(Functions.eachNElement(lxs, 2, 14) === LazyList(5, 3, 1))
  }

  test("eachNElement.limitSmallerThenStep") {
    val lxs = LazyList(5, 6, 3, 2, 1, 3, 4, 7, 9)
    assert(Functions.eachNElement(lxs, 4, 2) === LazyList(5))
  }

  test("eachNElement.bothBiggerThenListSize") {
    val lxs = LazyList(5, 6, 3, 2, 1, 3, 4, 7, 9)
    assert(Functions.eachNElement(lxs, 12, 15) === LazyList(5))
  }

  // Task5
  test("apply.oneEmptyAddition") {
    val res = Functions.apply(LazyList(), LazyList(1, 2, 3, 4))(_ + _)
    assert(Functions.apply(LazyList(), LazyList(1, 2, 3, 4))(_ + _) === LazyList(1, 2, 3, 4))
  }

  test("apply.addition") {
    assert(Functions.apply(LazyList(1, 2, 3, 7), LazyList(3, 5, 0, -2))(_ + _) === LazyList(4, 7, 3, 5))
  }

  test("apply.multiplication") {
    assert(Functions.apply(LazyList(1, 2, 3, 7), LazyList(3, 5, 0, -2))(_ * _) === LazyList(3, 10, 0, -14))
  }

  test("apply.divisionByZero") {
    assertThrows[ArithmeticException] {
      Functions.apply(LazyList(1, 2, 3, 7), LazyList(3, 5, 0, -2))(_ / _).force
    }
  }
}
