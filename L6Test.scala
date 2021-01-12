package List6Test
import org.scalatest.FunSuite
import L6.{ArrayComparer, BinaryTree, ListProduct, QuickSort}
import L6.BinaryTree.{Empty, Node}

class L6Test extends FunSuite {
  test ("Test of 'quickSort' function") {
    val array1 = Array(4.0, 8.0, 1.0, 12.0, 7.0, 3.0, 1.0, 9.0)
    QuickSort.quickSort(array1)
    assert(ArrayComparer.areArraysTheSame(array1, Array(1.0, 1.0, 3.0, 4.0, 7.0, 8.0, 9.0, 12.0)))

    val array2 = Array(-1.0, -8.0, 1.0, 186.0, 76.0, 32.0, 10.0, -8.0)
    QuickSort.quickSort(array2)
    assert(ArrayComparer.areArraysTheSame(array2, Array(-8.0, -8.0, -1.0, 1.0, 10.0, 32.0, 76.0, 186.0)))

    val array3 = Array(1.123, 1.011, 1.651, 1.124)
    QuickSort.quickSort(array3)
    assert(ArrayComparer.areArraysTheSame(array3, Array(1.011, 1.123, 1.124, 1.651)))
  }

  test("test of 'quickSortParallel' function") {
    val parallelArray1 = Array(4.0, 8.0, 1.0, 12.0, 7.0, 3.0, 1.0, 9.0)
    QuickSort.quickSortParallel(parallelArray1)
    assert(ArrayComparer.areArraysTheSame(parallelArray1, Array(1.0, 1.0, 3.0, 4.0, 7.0, 8.0, 9.0, 12.0)))

    val parallelArray2 = Array(-1.0, -8.0, 1.0, 186.0, 76.0, 32.0, 10.0, -8.0)
    QuickSort.quickSortParallel(parallelArray2)
    assert(ArrayComparer.areArraysTheSame(parallelArray2, Array(-8.0, -8.0, -1.0, 1.0, 10.0, 32.0, 76.0, 186.0)))

    val parallelArray3 = Array(1.123, 1.011, 1.651, 1.124)
    QuickSort.quickSortParallel(parallelArray3)
    assert(ArrayComparer.areArraysTheSame(parallelArray3, Array(1.011, 1.123, 1.124, 1.651)))
  }

  test ("Test of 'numberOfElementBT' function") {
    val tree1 = Node(1, Node(2, Empty(), Empty()), Node(1, Node(2, Node(3, Empty(), Empty()), Empty()), Empty()))
    val tree2 = Node("a", Node("b", Node("a", Empty(), Empty()), Empty()), Node("d", Empty(), Node("e", Empty(), Empty())))
    val tree3 = Node("a", Node("a", Node("a", Empty(), Empty()), Empty()), Node("a", Empty(), Node("a", Empty(), Empty())))
    val tree4 = Node(-6.7, Node(1.1, Empty(), Node(-6.7, Empty(), Empty())), Node(9.67, Empty(), Empty()))

    assert(BinaryTree.numberOfElementBT(tree1, 1) == 2)
    assert(BinaryTree.numberOfElementBT(tree1, 0) == 0)
    assert(BinaryTree.numberOfElementBT(tree2, "a") == 2)
    assert(BinaryTree.numberOfElementBT(tree2, "e") == 1)
    assert(BinaryTree.numberOfElementBT(tree3, "a") == 5)
    assert(BinaryTree.numberOfElementBT(tree4, -6.7) == 2)
  }

  test ("Test of 'numberOfElementBTParallel' function") {
    val tree1 = Node(1, Node(2, Empty(), Empty()), Node(1, Node(2, Node(3, Empty(), Empty()), Empty()), Empty()))
    val tree2 = Node("a", Node("b", Node("a", Empty(), Empty()), Empty()), Node("d", Empty(), Node("e", Empty(), Empty())))
    val tree3 = Node("a", Node("a", Node("a", Empty(), Empty()), Empty()), Node("a", Empty(), Node("a", Empty(), Empty())))
    val tree4 = Node(-6.7, Node(1.1, Empty(), Node(-6.7, Empty(), Empty())), Node(9.67, Empty(), Empty()))

    assert(BinaryTree.numberOfElementBTParallel(tree1, 1) == 2)
    assert(BinaryTree.numberOfElementBTParallel(tree1, 0) == 0)
    assert(BinaryTree.numberOfElementBTParallel(tree2, "a") == 2)
    assert(BinaryTree.numberOfElementBTParallel(tree2, "e") == 1)
    assert(BinaryTree.numberOfElementBTParallel(tree3, "a") == 5)
    assert(BinaryTree.numberOfElementBTParallel(tree4, -6.7) == 2)
  }

  test ("Test of 'listProduct' function") {
    assert(ListProduct.listProduct(List(1,2,3,4,5)) == 120)
    assert(ListProduct.listProduct(List(1,2,3,4,5,0)) == 0)
    assert(ListProduct.listProduct(List(2,2,3,4,5)) == 240)
    assert(ListProduct.listProduct(List(-2,-2,-3,4,5)) == -240)
    assert(ListProduct.listProduct(List(12,12,-1)) == -144)
    assert(ListProduct.listProduct(List(123,2,-3,4,-565)) == 1667880)
  }

  test ("Test of 'listProductParallel' function") {
    assert(ListProduct.listProductParallel(List(1,2,3,4,5)) == 120)
    assert(ListProduct.listProductParallel(List(1,2,3,4,5,0)) == 0)
    assert(ListProduct.listProductParallel(List(2,2,3,4,5)) == 240)
    assert(ListProduct.listProductParallel(List(-2,-2,-3,4,5)) == -240)
    assert(ListProduct.listProductParallel(List(12,12,-1)) == -144)
    assert(ListProduct.listProductParallel(List(123,2,-3,4,-565)) == 1667880)
  }

    test ("Test of 'areTheSameBT' function") {
      val tree1 = Node(1, Node(2, Empty(), Empty()), Node(1, Node(2, Node(3, Empty(), Empty()), Empty()), Empty()))
      val tree2 = Node("a", Node("b", Node("c", Empty(), Empty()), Empty()), Node("d", Empty(), Node("e", Empty(), Empty())))
      val tree3 = Node("a", Node("b", Node("c", Empty(), Empty()), Empty()), Node("a", Empty(), Node("e", Empty(), Empty())))
      val tree4 = Node(-6.7, Node(8.8, Empty(), Node(2.5, Empty(), Empty())), Node(9.67, Empty(), Empty()))
      val tree5 = Node(-6.7, Node(8.8, Empty(), Node(2.5, Empty(), Empty())), Node(9.67, Empty(), Empty()))
      val tree6 = Node(-7.7, Node(8.8, Empty(), Node(2.5, Empty(), Empty())), Node(9.67, Empty(), Empty()))
      val treeGenerated = BinaryTree.generateBT(7, 20)
      assert(BinaryTree.areTheSameBT(tree1, tree1))
      assert(! BinaryTree.areTheSameBT(tree2, tree3))
      assert(BinaryTree.areTheSameBT(tree2, tree2))
      assert(BinaryTree.areTheSameBT(tree3, tree3))
      assert(BinaryTree.areTheSameBT(tree4, tree4))
      assert(BinaryTree.areTheSameBT(tree4, tree5))
      assert(! BinaryTree.areTheSameBT(tree5, tree6))
      assert(BinaryTree.areTheSameBTParallel(treeGenerated, treeGenerated))
    }

    test ("Test of 'areTheSameBTParallel' function") {
      val tree1 = Node(1, Node(2, Empty(), Empty()), Node(1, Node(2, Node(3, Empty(), Empty()), Empty()), Empty()))
      val tree2 = Node("a", Node("b", Node("c", Empty(), Empty()), Empty()), Node("d", Empty(), Node("e", Empty(), Empty())))
      val tree3 = Node("a", Node("b", Node("c", Empty(), Empty()), Empty()), Node("a", Empty(), Node("e", Empty(), Empty())))
      val tree4 = Node(-6.7, Node(8.8, Empty(), Node(2.5, Empty(), Empty())), Node(9.67, Empty(), Empty()))
      val tree5 = Node(-6.7, Node(8.8, Empty(), Node(2.5, Empty(), Empty())), Node(9.67, Empty(), Empty()))
      val tree6 = Node(-7.7, Node(8.8, Empty(), Node(2.5, Empty(), Empty())), Node(9.67, Empty(), Empty()))
      val treeGenerated = BinaryTree.generateBT(7, 20)
      assert(BinaryTree.areTheSameBTParallel(tree1, tree1))
      assert(! BinaryTree.areTheSameBTParallel(tree2, tree3))
      assert(BinaryTree.areTheSameBTParallel(tree2, tree2))
      assert(BinaryTree.areTheSameBTParallel(tree3, tree3))
      assert(BinaryTree.areTheSameBTParallel(tree4, tree4))
      assert(BinaryTree.areTheSameBTParallel(tree4, tree5))
      assert(! BinaryTree.areTheSameBTParallel(tree5, tree6))
      assert(BinaryTree.areTheSameBTParallel(treeGenerated, treeGenerated))
    }
}

