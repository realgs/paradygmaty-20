import org.scalatest.FunSuite
import scala.util.Random

class l6_TreeDepthFull_Test extends FunSuite {
  test("TreeDepthFull_correctness") {
    val tree_1 = L6_tree.generateTree(3)(0)(10)
    assert(L6_tree.isTreeDepthN(tree_1)(3))
    assert(!L6_tree.isTreeDepthN(tree_1)(5))
    assert(L6_tree.isTreeFull(tree_1))

    val tree_2 = L6_tree.generateTree(10)(0)(10)
    assert(L6_tree.isTreeDepthN(tree_2)(10))
    assert(!L6_tree.isTreeDepthN(tree_2)(11))
    assert(L6_tree.isTreeFull(tree_2))

    val tree_3 = L6_tree.generateTree(20)(0)(10)
    assert(L6_tree.isTreeDepthN(tree_3)(20))
    assert(L6_tree.isTreeFull(tree_3))

    val tree_4 = L6_tree.getNotFullTree(10)(0)(15)
    assert(!L6_tree.isTreeFull(tree_4))
  }
}

class l6_TreeDepthFullPar_Test extends FunSuite {
  test("TreeDepthFullPar_correctness") {
    val tree_1 = L6_tree.generateTree(3)(0)(10)
    assert(L6_tree.isTreeDepthNPar(tree_1)(3))
    assert(!L6_tree.isTreeDepthNPar(tree_1)(5))
    assert(L6_tree.isTreeFullPar(tree_1))

    val tree_2 = L6_tree.generateTree(10)(0)(10)
    assert(L6_tree.isTreeDepthNPar(tree_2)(10))
    assert(!L6_tree.isTreeDepthNPar(tree_2)(12))
    assert(L6_tree.isTreeFullPar(tree_2))

    val tree_3 = L6_tree.generateTree(20)(0)(100)
    assert(L6_tree.isTreeDepthNPar(tree_3)(20))
    assert(L6_tree.isTreeFullPar(tree_3))

    val tree_4 = L6_tree.getNotFullTree(10)(0)(15)
    assert(!L6_tree.isTreeFullPar(tree_4))
  }
}

class l6_findPattern_Test extends FunSuite {
  test("find_List_Int") {
    assert(L6_pattern.find(List(1, 2), List(111, 212, 30301, 20)) == List(111, 212, 30301, 212, 20))
  }
  test("find_List_Example") {
    assert(L6_pattern.find(List("index0168"), List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"))
      == List("index0168202", "index0168211", "index0168210"))
  }
  test("find_List_Double") {
    assert(L6_pattern.find(List(2), List(1.0, 2.9, 37.92, 2.3)) == List(2.9, 37.92, 2.3))
  }
}

class l6_findPatternPar_Test extends FunSuite {
  test("find_List_Int") {
    assert(L6_pattern.findPar(List(1, 2), List(111, 212, 30301, 20)) == List(111, 212, 30301, 212, 20))
  }
  test("find_List_Example") {
    assert(L6_pattern.findPar(List("index0168"), List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"))
      == List("index0168202", "index0168211", "index0168210"))
  }
  test("find_List_Double") {
    assert(L6_pattern.findPar(List(2), List(1.0, 2.9, 37.92, 2.3)) == List(2.9, 37.92, 2.3))
  }
}

class l6_quicksort_Test extends FunSuite {
  test("Int") {
    val array_1 = Array(1, 6, 4, 5, 3)
    L6_quicksort.quicksort(array_1)
    assert(array_1.toList == List(1, 3, 4, 5, 6))
  }
  test("Int_repeating") {
    val array_2 = Array(-53, 6, 1, -3, -8, -8, 8, 8, 13, 83)
    L6_quicksort.quicksort(array_2)
    assert(array_2.toList == List(-53,-8, -8, -3, 1, 6, 8, 8, 13, 83))
  }
  val r = new Random()
  test("Random") {
    val array_3 = Array.fill(100)(r.nextInt())
    val list_4 = array_3.clone().toList.sorted(Ordering.Int)
    L6_quicksort.quicksort(array_3)
    assert(array_3.toList == list_4)
  }
}

class l6_quicksortPar_Test extends FunSuite {
  test("Int") {
    val array_1 = Array(1, 6, 4, 5, 3)
    L6_quicksort.quicksortPar(array_1)
    assert(array_1.toList == List(1, 3, 4, 5, 6))
  }
  test("Int_repeating") {
    val array_2 = Array(-53, 6, 1, -3, -8, -8, 8, 8, 13, 83)
    L6_quicksort.quicksortPar(array_2)
    assert(array_2.toList == List(-53,-8, -8, -3, 1, 6, 8, 8, 13, 83))
  }
}



