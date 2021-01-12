import Functions_L4.{Empty, Node, addition, breadthBT, breadthDeleteTheSameValues, checkIfNumbersAreInRange, checkIfTreeIsFull, depthDeleteTheSameValues, diffTrees, division, eachNElement, findDepth, generateTree, ldzialanie, multiplication, subtraction}

object Tests {

  def main(args: Array[String]): Unit = {
    task1_Test()
    task2_Test()
    task3_Test()
    task4_Test()
    task5_Test()
  }

  def task1_Test(): Unit = {
    println("Task 1 tests")
    val tr1 = generateTree(10, 10, 100)
    val tr2 = generateTree(0, 10, 100)
    val tr3 = generateTree(6, 0, 20009)
    //val tr4 = generateTree(-1, 0, 10)

    println(checkIfTreeIsFull(tr1))
    println(checkIfNumbersAreInRange(tr1, 10, 100))
    println(findDepth(tr1) == 10)

    println(checkIfTreeIsFull(tr2))
    println(checkIfNumbersAreInRange(tr2, 10, 100))
    println(findDepth(tr2) == 0)
    println(breadthBT(tr2) == List())

    println(checkIfTreeIsFull(tr3))
    println(checkIfNumbersAreInRange(tr3, 0, 20009))
    println(findDepth(tr3) == 6)
  }

  def task2_Test(): Unit = {
    println("\nTask 2 tests")
    val tree1 = generateTree(3, 10, 20)
    val tree2 = generateTree(3, 0, 10)
    println(diffTrees(tree1, tree2) != Empty)

    //val tree3 = generateTree(4, 0, 10)
    //diffTrees(tree1, tree3)

    val tree4 = Node(10, Node(20, Node(30, Empty, Empty), Node(40, Empty, Empty))
      , Node(50, Node(60, Empty, Empty), Node(70, Empty, Empty)))
    val tree5 = Node(1, Node(2, Node(3, Empty, Empty), Node(4, Empty, Empty))
      , Node(5, Node(6, Empty, Empty), Node(7, Empty, Empty)))
    println(diffTrees(tree4, tree5) == Node(9, Node(18, Node(27, Empty, Empty), Node(36, Empty, Empty))
      , Node(45, Node(54, Empty, Empty), Node(63, Empty, Empty))))
    println(diffTrees(tree5, tree4) == Node(-9, Node(-18, Node(-27, Empty, Empty), Node(-36, Empty, Empty))
      , Node(-45, Node(-54, Empty, Empty), Node(-63, Empty, Empty))))
  }

  def task3_Test(): Unit = {
    println("\nTask 3 tests")
    val t1 = Node(1, Node(2, Node(2, Empty, Empty), Node(2, Empty, Empty)), Node(3, Node(2, Empty, Empty), Node(1, Empty, Empty)))
    val t2 = Node(1, Node(1, Node(1, Empty, Empty), Node(1, Empty, Empty)), Node(2, Node(3, Empty, Empty), Node(1, Empty, Empty)))
    val t3 = Node(1, Node(2, Node(3, Empty, Empty), Node(4, Empty, Empty)), Node(7, Node(9, Empty, Empty), Node(10, Empty, Empty)))
    val t4 = Node(1, Node(2, Node(3, Empty, Empty), Node(4, Empty, Empty)), Node(7, Node(8, Empty, Empty), Node(10, Empty, Empty)))

    val t5 = depthDeleteTheSameValues(t3, t4)
    //depthDeleteTheSameValues(t5._1, t2)
    //breadthDeleteTheSameValues(t5._1, t2)
    println(depthDeleteTheSameValues(t1, t2) == (Node(-1,Node(2,Node(2,Empty,Empty),Node(2,Empty,Empty)),Node(3,Node(2,Empty,Empty),Empty)),Node(-1,Node(1,Node(1,Empty,Empty),Node(1,Empty,Empty)),Node(2,Node(3,Empty,Empty),Empty))))
    println(breadthDeleteTheSameValues(t1, t2) == (Node(-1,Node(2,Node(2,Empty,Empty),Node(2,Empty,Empty)),Node(3,Node(2,Empty,Empty),Empty)),Node(-1,Node(1,Node(1,Empty,Empty),Node(1,Empty,Empty)),Node(2,Node(3,Empty,Empty),Empty))))
    println(depthDeleteTheSameValues(t3, t4) == (Node(-1,Empty,Node(-1,Node(9,Empty,Empty),Empty)),Node(-1,Empty,Node(-1,Node(8,Empty,Empty),Empty))))
    println(breadthDeleteTheSameValues(t3, t4) == (Node(-1,Empty,Node(-1,Node(9,Empty,Empty),Empty)),Node(-1,Empty,Node(-1,Node(8,Empty,Empty),Empty))))
  }

  def task4_Test(): Unit = {
    println("\nTask 4 tests")
    println(eachNElement(LazyList(1, 2, 3, 4, 5), 2, 4).toList == List(1, 3))
    println(eachNElement(LazyList(1, 2, 3, 4, 5), 0, 4).toList == List())
    println(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8), 3, 9).toList == List(1, 4, 7))
    println(eachNElement(LazyList("pa", "ram", "pam", "pam", "ram"), 3, 3).toList == List("pa"))
    println(eachNElement(LazyList(1.2, 3.4, 5.6, 7.8, 9), 1, 5).toList == List(1.2, 3.4, 5.6, 7.8, 9))
    println(eachNElement(LazyList(), 10, 100) == LazyList())
  }

  def task5_Test(): Unit = {
    println("\nTask 5 tests")
    println(ldzialanie(LazyList(10, 20, 30, 40), LazyList(1, 2, 3, 4), subtraction).toList == List(9, 18, 27, 36))
    println(ldzialanie(LazyList(10, 20, 30, 40, 50), LazyList(1, 2, 3, 4), subtraction).toList == List(9, 18, 27, 36, 50))
    println(ldzialanie(LazyList(0, 1, 2.5, 3), LazyList(100, 100, 100, 100, 100), multiplication).toList == List(0, 100, 250, 300, 100))
    println(ldzialanie(LazyList(100, 100, 100, 100), LazyList(1, 2, 4, 200), division).toList == List(100, 50, 25, 0.5))
    // println(ldzialanie(LazyList(0), LazyList(0), division))
    println(ldzialanie(LazyList(), LazyList(), addition) == LazyList())
    println(ldzialanie(LazyList(1, 2, 3), LazyList(), addition) == LazyList(1, 2, 3))
    println(ldzialanie(LazyList(), LazyList(1000, 1, 2), addition) == LazyList(1000, 1, 2))
  }
}
