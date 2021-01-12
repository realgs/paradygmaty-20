import trees._

object treesTest {

  val tree1: trees.BT[Int] = fullTree(2, 1, 10000)
  val tree2: trees.BT[Int] = fullTree(10, 5, 10)
  val tree3: trees.BT[Int] = fullTree(0, 1, 10)


  def zad1Test(): Unit = {
    println("\nfullTree test")
    println(treeHeight(tree1) == 2)
    println(treeHeight(tree3) == 0)
    println(treeHeight(tree2) == 10)
    println(isFull(tree1))
    println(isFull(tree3))
    println(isFull(tree2))
    println(isInBound(tree1, 1, 10000))
    println(isInBound(tree2, 5, 10))
  }

  def zad2Test(): Unit = {
    println("\ntree subtraction test")
    val a = Node(1, Node(2, Node(3, Empty, Empty), Node(4, Empty, Empty)), Node(4, Node(6, Empty, Empty), Node(7, Empty, Empty)))
    val b = Node(7, Node(6, Node(5, Empty, Empty), Node(4, Empty, Empty)), Node(3, Node(2, Empty, Empty), Node(1, Empty, Empty)))
    val result1 = Node(-6, Node(-4, Node(-2, Empty, Empty), Node(0, Empty, Empty)), Node(1, Node(4, Empty, Empty), Node(6, Empty, Empty)))
    println(treeSubtraction(a, b) == result1)
    val c = Node(2, Empty, Empty)
    val d = c
    val result2 = Node(0, Empty, Empty)
    println(treeSubtraction(c, d) == result2)
    val e = Empty
    val f = Empty
    println(treeSubtraction(e, f) == Empty)
  }

  def zad3Test(): Unit = {
    println("\nremove repetition test")
    val a = Node(3, Node(4, Node(2, Empty, Empty), Node(12, Empty, Empty)), Node(10, Node(2, Empty, Empty), Node(5, Empty, Empty)))
    val b = Node(1, Node(5, Node(2, Empty, Empty), Node(12, Empty, Empty)), Node(10, Node(1, Empty, Empty), Node(5, Empty, Empty)))
    val result1 = (Node(3, Node(4, Empty, Empty), Node(-1, Node(2, Empty, Empty), Empty)),
      Node(1, Node(5, Empty, Empty), Node(-1, Node(1, Empty, Empty), Empty)))
    println(removeRepetitionBFS(a, b) == result1)
    println(removeRepetitionDFS(a, b) == result1)
    val c = Node(3, Node(4, Empty, Empty), Node(10, Empty, Empty))
    val d = Node(1, Node(4, Empty, Empty), Node(10, Empty, Empty))
    val result2 = (Node(3, Empty, Empty), Node(1, Empty, Empty))
    println(removeRepetitionBFS(c, d) == result2)
    println(removeRepetitionDFS(c, d) == result2)
    val e = Empty
    val f = Empty
    println(removeRepetitionBFS(e, f) == (Empty, Empty))
    println(removeRepetitionDFS(e, f) == (Empty, Empty))

  }


}

