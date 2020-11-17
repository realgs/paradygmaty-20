import Functions.{Empty, Node, eachNElement, findTreesDifference, generateTree, ldzialanie, removeDuplicatesBFS, removeDuplicatesDFS}
import Utils.{fib, isTreeFull}

object Tests {
  def main(args: Array[String]): Unit = {
    // Task 1
    println(generateTree(0, 1, 2) == Empty)
    println(generateTree(1, -1, 0) == Empty)
    println(generateTree(1, 0, -1) == Empty)
    println(generateTree(1, 1, 1) == Empty)
    println(generateTree(1, 1, 2) == Node(1, Empty, Empty))
    println(generateTree(2, 1, 2) == Node(1, Node(1, Empty, Empty), Node(1, Empty, Empty)))
    println(generateTree(3, 1, 2) == Node(1,Node(1,Node(1,Empty,Empty),Node(1,Empty,Empty)),Node(1,Node(1,Empty,Empty),Node(1,Empty,Empty))))

    println(!isTreeFull(Node(1, Empty, Node(2, Empty, Empty))))
    println(!isTreeFull(Node(2, Node(3, Empty, Node(1, Empty, Empty)), Node(5, Empty, Empty))))

    println(isTreeFull(generateTree(1, 1, 2)))
    println(isTreeFull(generateTree(2, 1, 8)))
    println(isTreeFull(generateTree(3, 30, 99)))
    println(isTreeFull(generateTree(4, 4, 5)))

    // Task2
    val emptyTree = generateTree(-1, 1, 2)
    println(findTreesDifference(emptyTree, emptyTree) == Empty)

    val tree1 = generateTree(2, 1, 2)
    println(findTreesDifference(tree1, tree1) == Node(0, Node(0, Empty, Empty), Node(0, Empty, Empty)))

    val tree3 = generateTree(3, 1, 5)
    val tree4 = generateTree(3, 4, 8)
//    println("t3", tree3)
//    println("t4", tree4)

    println(findTreesDifference(tree3, tree4) != (Empty, Empty))
    println(findTreesDifference(tree4, tree3) != (Empty, Empty))

    val tree5 = generateTree(3, 1, 99)
    val tree6 = generateTree(3, 1, 99)
//    println("t5", tree5)
//    println("t6", tree6)

    println(findTreesDifference(tree5, tree6) != (Empty, Empty))
    println(findTreesDifference(tree6, tree5) != (Empty, Empty))

    val tree7 = Node(2,Node(2,Node(2,Empty,Empty),Node(1,Empty,Empty)),Node(2,Node(1,Empty,Empty),Node(2,Empty,Empty)))
    val tree8 = Node(1,Node(1,Node(1,Empty,Empty),Node(0,Empty,Empty)),Node(1,Node(0,Empty,Empty),Node(1,Empty,Empty)))

    println(findTreesDifference(tree7, tree8) == Node(1,Node(1,Node(1,Empty,Empty),Node(1,Empty,Empty)),Node(1,Node(1,Empty,Empty),Node(1,Empty,Empty))))

//    findTreesDifference(tree1, tree5) // exception

    // Task 3
    // DFS
    val t1 = Node(2,Node(1, Empty, Empty), Node(2, Empty, Empty))
    val t2 = Node(2,Node(2, Empty, Empty), Node(2, Empty, Empty))

    println(removeDuplicatesDFS(t1, t1) == (Empty, Empty))
    println(removeDuplicatesDFS(t2, t2) == (Empty, Empty))
    println(removeDuplicatesDFS(generateTree(5, 5, 6), generateTree(5, 5, 6)) == (Empty, Empty))

    val result1 = removeDuplicatesDFS(t1, t2)
    println(result1._1 == Node(-1, Node(1, Empty, Empty), Empty))
    println(result1._2 == Node(-1, Node(2, Empty, Empty), Empty))

    val t3 = Node(2,Node(2,Node(2,Empty,Empty),Node(1,Empty,Empty)),Node(2,Node(1,Empty,Empty),Node(2,Empty,Empty)))
    val t4 = Node(1,Node(2,Node(1,Empty,Empty),Node(1,Empty,Empty)),Node(1,Node(1,Empty,Empty),Node(1,Empty,Empty)))

    val result2 = removeDuplicatesDFS(t3, t4)
    println(result2._1 == Node(2,Node(-1,Node(2,Empty,Empty),Empty),Node(2,Empty,Node(2,Empty,Empty))))
    println(result2._2 == Node(1,Node(-1,Node(1,Empty,Empty),Empty),Node(1,Empty,Node(1,Empty,Empty))))

    val t5 = generateTree(3, 1, 3)
    val t6 = generateTree(3, 1, 3)
//    println(t5)
//    println(t6)
//    println(removeDuplicatesDFS(t5, t6))

//    val oddTree1 = Node(1, Node(2, Empty, Empty), Empty)
//    val oddTree2 = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))
//    println(removeDuplicatesDFS(oddTree1, oddTree2)) // exception

    // BFS
    println(removeDuplicatesBFS(t1, t1) == (Empty, Empty))
    println(removeDuplicatesBFS(t3, t3) == (Empty, Empty))
    println(removeDuplicatesBFS(generateTree(5, 5, 6), generateTree(5, 5, 6)) == (Empty, Empty))

    val result3 = removeDuplicatesBFS(t1, t2)
    println(result3._1 == Node(-1, Node(1, Empty, Empty), Empty))
    println(result3._2 == Node(-1, Node(2, Empty, Empty), Empty))

    val result4 = removeDuplicatesBFS(t3, t4)
    println(result4._1 == Node(2,Node(-1,Node(2,Empty,Empty),Empty),Node(2,Empty,Node(2,Empty,Empty))))
    println(result4._2 == Node(1,Node(-1,Node(1,Empty,Empty),Empty),Node(1,Empty,Node(1,Empty,Empty))))

    println(removeDuplicatesDFS(t3, t4) == removeDuplicatesBFS(t3, t4))
    println(removeDuplicatesBFS(t5, t6) == removeDuplicatesBFS(t5, t6))

    // Task 4
    println(eachNElement(LazyList(), 2, 3).toList == List())
    println(eachNElement(LazyList(1, 2, 3), 0, 3).toList == List())
    println(eachNElement(LazyList(1, 2, 3), 2, 0).toList == List())

    println(fib.take(16).toList)
    println(eachNElement(fib.take(16), 3, 12).toList == List(0, 2, 8, 34))
    println(eachNElement(fib.take(16), 2, 4).toList == List(0, 1))
    println(eachNElement(fib.take(30), 5, 15).toList == List(0, 5, 55))
    println(eachNElement(fib.take(30), 5, 16).toList == List(0, 5, 55, 610))

    println(eachNElement(LazyList("What", "time", "is", "it"), 1, 4).toList == List("What", "time", "is", "it"))
    println(eachNElement(LazyList("Test", "tes", "te", "t", ""), 2, 4).toList == List("Test", "te"))
    println(eachNElement(LazyList("Test", "tes", "te", "t", ""), 2, 5).toList == List("Test", "te", ""))

    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3).toList == List(5, 3))
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4).toList == List(5, 3))

    println(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 3, 13).toList == List(1, 4, 7, 10, 13))

    // Task 5
    println(ldzialanie(LazyList(), LazyList(), Utils.*).toList == List())
    println(ldzialanie(LazyList(1.22, 2.33, 3.44), LazyList(), Utils.+).toList == List(1.22, 2.33, 3.44))
    println(ldzialanie(LazyList(), LazyList(1.22, 2.33, 3.44), Utils.-).toList == List(1.22, 2.33, 3.44))

    println(ldzialanie(LazyList(1.0, 2.0, 3.0), LazyList(2.0, 3.0, 4.0, 5.0), Utils.+).toList == List(3.0, 5.0, 7.0, 5.0))
    println(ldzialanie(LazyList(1.4, 2.25, 3.75), LazyList(1.4, 2.0, 2.25, 5.0, 7.5), Utils.-).toList == List(0.0, 0.25, 1.5, 5.0, 7.5))
    println(ldzialanie(LazyList(1.5, 2.25, 3.75), LazyList(2.5, 3.25, 4, 5.125), Utils.*).toList == List(3.75, 7.3125, 15.0, 5.125))
    println(ldzialanie(LazyList(1.0, 2.5, 3.0), LazyList(2.0, 2.0, 4.0, 5.0), Utils./).toList == List(0.5, 1.25, 0.75, 5))

    println(ldzialanie(LazyList(1, 2, 3, 4), LazyList(2, 4, 6), Utils.add).toList == List(3, 6, 9, 4))
    println(ldzialanie(LazyList(1, 2, 3, 4), LazyList(2, 4, 6), Utils.sub).toList == List(-1, -2, -3, 4))
    println(ldzialanie(LazyList(1, 2, 3, 4), LazyList(2, 4, 6), Utils.mul).toList == List(2, 8, 18, 4))
    println(ldzialanie(LazyList(8, 4, 12, 4), LazyList(2, 4, 6), Utils.div).toList == List(4, 1, 2, 4))

    println(ldzialanie(LazyList("Ti", "t", "d", "i"), LazyList("me", "o", "o", "t"), Utils.^).toList == List("Time", "to", "do", "it"))
  }
}
