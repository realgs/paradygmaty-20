import org.scalatest.funsuite.AnyFunSuite

class BinaryTrees_Test extends AnyFunSuite {

  val binaryTrees = new BinaryTrees

  //zadanie 1
  test("Test depth; treeGenerator function") {
    assert(binaryTrees.maxDepth(binaryTrees.treeGenerator(3, 10)) == 3)
  }

  test("Test isFull; treeGenerator function") {
    assert(binaryTrees.countLeafs(binaryTrees.treeGenerator(3, 10)) == 8)
  }

  test("Test root; treesSubtraction function") {
    val t1 = binaryTrees.treeGenerator(2, 10)
    val t2 = binaryTrees.treeGenerator(2, 10)
    val res = binaryTrees.treesSubtraction(t1, t2)
    assert(binaryTrees.rootValue(res) == binaryTrees.rootValue(t1) - binaryTrees.rootValue(t2))
  }

  test("Test full trees; treesSubtraction function") {
    val t1 = binaryTrees.Node(10, binaryTrees.Leaf(20), binaryTrees.Leaf(30))
    val t2 = binaryTrees.Node(1, binaryTrees.Leaf(2), binaryTrees.Leaf(3))
    val res = binaryTrees.treesSubtraction(t1, t2)
    assert(res == binaryTrees.Node(9, binaryTrees.Leaf(18), binaryTrees.Leaf(27)))
  }

  test("Test exception; treesSubtraction function") {
    assertThrows[IllegalArgumentException] {
      val t1 = binaryTrees.treeGenerator(2, 10)
      val t2 = binaryTrees.treeGenerator(3, 10)
      binaryTrees.treesSubtraction(t1, t2)
    }
  }

  test("Test deleteCopiesDFS function") {
    val t1 = binaryTrees.Node(1,binaryTrees.Node(5,binaryTrees.Leaf(2),binaryTrees.Leaf(8)),binaryTrees.Node(9,binaryTrees.Leaf(6),binaryTrees.Leaf(4)))
    val t2 = binaryTrees.Node(1,binaryTrees.Node(5,binaryTrees.Leaf(7),binaryTrees.Leaf(9)),binaryTrees.Node(9,binaryTrees.Leaf(6),binaryTrees.Leaf(4)))

    val r1 = binaryTrees.deleteCopiesDFS(t1, t2)._1
    val r2 = binaryTrees.deleteCopiesDFS(t1, t2)._2
    assert(r1 == binaryTrees.Node(-1,binaryTrees.Node(-1,binaryTrees.Leaf(2),binaryTrees.Leaf(8)),binaryTrees.Empty))
    assert(r2 == binaryTrees.Node(-1,binaryTrees.Node(-1,binaryTrees.Leaf(7),binaryTrees.Leaf(9)),binaryTrees.Empty))
  }

  test("Test exception; deleteCopiesDFS function") {
    assertThrows[IllegalArgumentException] {
      val t1 = binaryTrees.treeGenerator(2, 10)
      val t2 = binaryTrees.treeGenerator(3, 10)
      binaryTrees.deleteCopiesDFS(t1, t2)
    }
  }

  test("Test deleteCopiesBFS function") {
    val t1 = binaryTrees.Node(1,binaryTrees.Node(5,binaryTrees.Leaf(2),binaryTrees.Leaf(8)),binaryTrees.Node(9,binaryTrees.Leaf(6),binaryTrees.Leaf(4)))
    val t2 = binaryTrees.Node(1,binaryTrees.Node(5,binaryTrees.Leaf(7),binaryTrees.Leaf(9)),binaryTrees.Node(9,binaryTrees.Leaf(6),binaryTrees.Leaf(4)))

    val r1 = binaryTrees.deleteCopiesBFS(t1, t2)._1
    val r2 = binaryTrees.deleteCopiesBFS(t1, t2)._2
    assert(r1 == binaryTrees.Node(-1,binaryTrees.Node(-1,binaryTrees.Leaf(2),binaryTrees.Leaf(8)),binaryTrees.Empty))
    assert(r2 == binaryTrees.Node(-1,binaryTrees.Node(-1,binaryTrees.Leaf(7),binaryTrees.Leaf(9)),binaryTrees.Empty))
  }

  test("Test exception; deleteCopiesBFS function") {
    assertThrows[IllegalArgumentException] {
      val t1 = binaryTrees.treeGenerator(2, 10)
      val t2 = binaryTrees.treeGenerator(3, 10)
      binaryTrees.deleteCopiesBFS(t1, t2)
    }
  }

}
