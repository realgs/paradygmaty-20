import Functions._
import Utils._

object Tests {

  def main(args: Array[String]): Unit = {

    // Task 1
    println(generateBinaryTree(0, (1, 2)) == Empty)
    println(generateBinaryTree(1, (1, 1)) == Node(1, Empty, Empty))
    println(generateBinaryTree(2, (1, 1)) == Node(1, Node(1, Empty, Empty), Node(1, Empty, Empty)))
    println(generateBinaryTree(3, (1, 1)) == Node(1, Node(1, Node(1, Empty, Empty), Node(1, Empty, Empty)), Node(1, Node(1, Empty, Empty), Node(1, Empty, Empty))))
    val t = generateBinaryTree(1, (1, 2))
    println(t == Node(1, Empty, Empty) || t == Node(2, Empty, Empty))
    println(isFullTree(generateBinaryTree(4, (4, 199))))
    println(isFullTree(generateBinaryTree(7, (20, 34))))
    println(isFullTree(generateBinaryTree(2, (67, 5000))))
    println(isFullTree(generateBinaryTree(1, (100, 4513))))

    // Task 2
    val tree2_1 = Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(6, Node(7, Empty, Empty), Node(10, Empty, Empty)))
    val tree2_2 = Node(4, Node(3, Node(2, Empty, Empty), Node(1, Empty, Empty)), Node(7, Node(9, Empty, Empty), Node(20, Empty, Empty)))
    val tree2_3 = Node(-3, Node(-1, Node(2, Empty, Empty), Node(4, Empty, Empty)), Node(-1, Node(-2, Empty, Empty), Node(-10, Empty, Empty)))
    val tree2_4 = Node(3, Node(1, Node(-2, Empty, Empty), Node(-4, Empty, Empty)), Node(1, Node(2,Empty, Empty), Node(10, Empty, Empty)))
    val tree2_5 = Node(1, Node(2, Node(4, Node(4, Empty, Empty), Node(4, Empty, Empty)), Node(5, Node(4, Empty, Empty), Node(4, Empty, Empty))), Node(6, Node(7, Node(8, Empty, Empty), Node(9, Empty, Empty)), Node(10, Node(11, Empty, Empty), Node(12, Empty, Empty))))
    val tree2_6 = Node(4, Node(3, Node(2, Node(4, Empty, Empty), Node(4, Empty, Empty)), Node(1, Node(4, Empty, Empty), Node(4, Empty, Empty))), Node(7, Node(9, Node(11, Empty, Empty), Node(13, Empty, Empty)), Node(20, Node(31, Empty, Empty), Node(2, Empty, Empty))))
    val tree2_7 = Node(-3, Node(-1, Node(2, Node(0, Empty, Empty), Node(0, Empty, Empty)), Node(4, Node(0, Empty, Empty), Node(0, Empty, Empty))), Node(-1, Node(-2, Node(-3, Empty, Empty), Node(-4, Empty, Empty)), Node(-10, Node(-20, Empty, Empty), Node(10, Empty, Empty))))
    val tree2_8 = Node(3, Node(1, Node(-2, Node(0, Empty, Empty), Node(0, Empty, Empty)), Node(-4, Node(0, Empty, Empty), Node(0, Empty, Empty))), Node(1, Node(2, Node(3, Empty, Empty), Node(4, Empty, Empty)), Node(10, Node(20, Empty, Empty), Node(-10, Empty, Empty))))

    println(subtractTrees(tree2_1, tree2_2) == tree2_3)
    println(subtractTrees(tree2_2, tree2_1) == tree2_4)
    println(subtractTrees(tree2_1, tree2_3) == tree2_2)
    println(subtractTrees(tree2_2, tree2_4) == tree2_1)
    println(subtractTrees(tree2_5, tree2_6) == tree2_7)
    println(subtractTrees(tree2_6, tree2_5) == tree2_8)
    println(subtractTrees(tree2_5, tree2_7) == tree2_6)
    println(subtractTrees(tree2_6, tree2_8) == tree2_5)
    println(subtractTrees(generateBinaryTree(3, (1, 1)), generateBinaryTree(3, (1, 1))) == Node(0, Node(0, Node(0, Empty, Empty), Node(0, Empty, Empty)), Node(0, Node(0, Empty, Empty), Node(0, Empty, Empty))))
    println(subtractTrees(generateBinaryTree(1, (4, 4)), generateBinaryTree(1, (2, 2))) == Node(2, Empty, Empty))
    println(subtractTrees(Empty, Empty) == Empty)

    // Task 3
    val tree3_1 = Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(6, Node(7, Empty, Empty), Node(10, Empty, Empty)))
    val tree3_2 = Node(2, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(7, Node(8, Empty, Empty), Node(10, Empty, Empty)))
    val tree3_3 = Node(1, Empty, Node(6, Node(7, Empty, Empty), Empty))
    val tree3_4 = Node(2, Empty, Node(7, Node(8, Empty, Empty), Empty))

    val tree3_5 = Node(4, Node(3, Node(2, Node(4, Empty, Empty), Node(4, Empty, Empty)), Node(1, Node(4, Empty, Empty), Node(4, Empty, Empty))), Node(7, Node(9, Node(11, Empty, Empty), Node(13, Empty, Empty)), Node(20, Node(31, Empty, Empty), Node(2, Empty, Empty))))
    val tree3_6 = Node(5, Node(4, Node(2, Node(4, Empty, Empty), Node(4, Empty, Empty)), Node(2, Node(5, Empty, Empty), Node(4, Empty, Empty))), Node(7, Node(8, Node(12, Empty, Empty), Node(13, Empty, Empty)), Node(20, Node(31, Empty, Empty), Node(3, Empty, Empty))))
    val tree3_7 = Node(4, Node(3, Empty, Node(1, Node(4, Empty, Empty), Empty)), Node(-1, Node(9, Node(11, Empty, Empty), Empty), Node(-1, Empty, Node(2, Empty, Empty))))
    val tree3_8 = Node(5, Node(4, Empty, Node(2, Node(5, Empty, Empty), Empty)), Node(-1, Node(8, Node(12, Empty, Empty), Empty), Node(-1, Empty, Node(3, Empty, Empty))))

    val tree3_9 = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))
    val tree3_10 = Node(1, Node(3, Empty, Empty), Node(2, Empty, Empty))
    val tree3_11 = Node(-1, Node(2, Empty, Empty), Node(3, Empty, Empty))
    val tree3_12 = Node(-1, Node(3, Empty, Empty), Node(2, Empty, Empty))

    println(deleteDuplicatesDFS(tree3_1, tree3_2) == (tree3_3, tree3_4))
    println(deleteDuplicatesDFS(tree3_5, tree3_6) == (tree3_7, tree3_8))
    println(deleteDuplicatesDFS(tree3_9, tree3_10) == (tree3_11, tree3_12))
    println(deleteDuplicatesDFS(tree3_1, tree3_1) == (Empty, Empty))
    println(deleteDuplicatesDFS(tree3_11, tree3_11) == (Empty, Empty))
    println(deleteDuplicatesDFS(Empty, Empty) == (Empty, Empty))

    // Task 4
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3).toList == List(5, 3))
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4).toList == List(5, 3))
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 5).toList == List(5, 3, 1))
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 100).toList == List(5, 3, 1))
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 1, 1).toList == List(5))
    println(eachNElement(LazyList(), 3, 500).toList == Nil)
//    println(eachNElement(LazyList(5, 6, 3, 2, 1), 0, 3))
//    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 0))

    println(eachNElement(intLazyListGenerator().take(15), 3, 10).toList == List(1, 4, 7, 10))
    println(eachNElement(intLazyListGenerator().take(6), 1, 1).toList == List(1))
    println(eachNElement(intLazyListGenerator().take(6), 1, 10).toList == List(1, 2, 3, 4, 5, 6))

    println(eachNElement(LazyList("5", "6", "3"), 2, 3).toList == List("5", "3"))
    println(eachNElement(LazyList("5", "6", "3", "2"), 2, 4).toList == List("5", "3"))
    println(eachNElement(LazyList("5"), 2, 5).toList == List("5"))
    println(eachNElement(LazyList("5", "6"), 2, 100).toList == List("5"))
    println(eachNElement(LazyList("5", "6", "3", "2", "1"), 1, 1).toList == List("5"))
    println(eachNElement(LazyList(), 3, 20).toList == Nil)
//    println(eachNElement(LazyList("5", "6", "3", "2", "1"), 0, 3))
//    println(eachNElement(LazyList("5", "6", "3", "2", "1"), 2, 0))

    // Task 5
    println(ldzialanie(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), (x, y) => x + y).toList == List(3, 5, 7, 5))
    println(ldzialanie(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), (x, y) => x - y).toList == List(-1, -1, -1, 5))
    println(ldzialanie(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), (x, y) => x * y).toList == List(2, 6, 12, 5))
    println(ldzialanie(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), (x, y) => x / y).toList == List(0.5, 2.0 / 3.0, 0.75, 5))
//    println(ldzialanie(LazyList(1, 2, 3), LazyList(0, 0), (x, y) => { if(y == 0) throw new IllegalArgumentException("Division by 0!") else x / y }).toList)

    println(ldzialanie(LazyList(1132.0124, -124.0002, 0.0, 0.000042, -1), LazyList(15, 15674.42, -0.89), (x, y) => x + y).toList == List(1132.0124 + 15, -124.0002 + 15674.42, 0.0 + -0.89, 0.000042, -1))
    println(ldzialanie(LazyList(1132.0124, -124.0002, 0.0, 0.000042, -1), LazyList(15, 15674.42, -0.89), (x, y) => x - y).toList == List(1132.0124 - 15, -124.0002 - 15674.42, 0.0 - -0.89, 0.000042, -1))
    println(ldzialanie(LazyList(1132.0124, -124.0002, 0.0, 0.000042, -1), LazyList(15, 15674.42, -0.89), (x, y) => x * y).toList == List(1132.0124 * 15, -124.0002 * 15674.42, 0.0 * -0.89, 0.000042, -1))
    println(ldzialanie(LazyList(1132.0124, -124.0002, 0.0, 0.000042, -1), LazyList(15, 15674.42, -0.89), (x, y) => x / y).toList == List(1132.0124 / 15, -124.0002 / 15674.42, 0.0 / -0.89, 0.000042, -1))
    println(ldzialanie(LazyList(), LazyList(), (x, y) => x + y).toList == Nil)
  }
}
