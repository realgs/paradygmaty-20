import org.scalatest.funsuite.AnyFunSuite

class BinaryTrees_Test extends AnyFunSuite {

  val BT = new BinaryTrees

  //Zadanie 1
  test("Test depth; treeGenerator function") {
    assert(BT.maxDepth(BT.treeGenerator(3, 10)) == 3)
  }

  test("Test isFull; treeGenerator function") {
    assert(BT.countLeafs(BT.treeGenerator(3, 10)) == 8)
  }

  //Zadanie 2
  test("Test root; treesSubtraction function") {
    val t1 = BT.treeGenerator(2, 10)
    val t2 = BT.treeGenerator(2, 10)
    val res = BT.treesSubtraction(t1, t2)
    assert(BT.rootValue(res) == BT.rootValue(t1) - BT.rootValue(t2))
  }

  test("Test full trees; treesSubtraction function") {
    val t1 = BT.Node(10, BT.Leaf(20), BT.Leaf(30))
    val t2 = BT.Node(1, BT.Leaf(2), BT.Leaf(3))
    val res = BT.treesSubtraction(t1, t2)
    assert(res == BT.Node(9, BT.Leaf(18), BT.Leaf(27)))
  }

  test("Test exception; treesSubtraction function") {
    assertThrows[IllegalArgumentException] {
      val t1 = BT.treeGenerator(2, 10)
      val t2 = BT.treeGenerator(3, 10)
      BT.treesSubtraction(t1, t2)
    }
  }

  //Zadanie 3
  test("Test deleteCopiesDFS function") {
    val t1 = BT.Node(1, BT.Node(5, BT.Leaf(2), BT.Leaf(8)), BT.Node(9, BT.Leaf(6), BT.Leaf(4)))
    val t2 = BT.Node(1, BT.Node(5, BT.Leaf(7), BT.Leaf(9)), BT.Node(9, BT.Leaf(6), BT.Leaf(4)))

    val r1 = BT.deleteCopiesDFS(t1, t2)._1
    val r2 = BT.deleteCopiesDFS(t1, t2)._2
    assert(r1 == BT.Node(-1, BT.Node(-1, BT.Leaf(2), BT.Leaf(8)), BT.Empty))
    assert(r2 == BT.Node(-1, BT.Node(-1, BT.Leaf(7), BT.Leaf(9)), BT.Empty))
  }

  test("Test exception; deleteCopiesDFS function") {
    assertThrows[IllegalArgumentException] {
      val t1 = BT.treeGenerator(2, 10)
      val t2 = BT.treeGenerator(3, 10)
      BT.deleteCopiesDFS(t1, t2)
    }
  }

  test("Test deleteCopiesBFS function") {
    val t1 = BT.Node(1, BT.Node(5, BT.Leaf(2), BT.Leaf(8)), BT.Node(9, BT.Leaf(6), BT.Leaf(4)))
    val t2 = BT.Node(1, BT.Node(5, BT.Leaf(7), BT.Leaf(9)), BT.Node(9, BT.Leaf(6), BT.Leaf(4)))

    val r1 = BT.deleteCopiesBFS(t1, t2)._1
    val r2 = BT.deleteCopiesBFS(t1, t2)._2
    assert(r1 == BT.Node(-1, BT.Node(-1, BT.Leaf(2), BT.Leaf(8)), BT.Empty))
    assert(r2 == BT.Node(-1, BT.Node(-1, BT.Leaf(7), BT.Leaf(9)), BT.Empty))
  }

  test("Test exception; deleteCopiesBFS function") {
    assertThrows[IllegalArgumentException] {
      val t1 = BT.treeGenerator(2, 10)
      val t2 = BT.treeGenerator(3, 10)
      BT.deleteCopiesBFS(t1, t2)
    }
  }

}
