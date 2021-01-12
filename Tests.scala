import Functions.generateTree, Functions.generateDifferTree, Functions.generateNoDuplicatesTreeDFS,
Functions.eachNElement, Functions.ldzialanie

object Tests {
  def main(args: Array[String]): Unit =
    {
      // Zadanie 1 Testy
      zadanie1_tests()

      // Zadanie 2 Testy
      zadanie2_tests()

      // Zadanie 3 Testy
      zadanie3_tests()

      // Zadanie 4 Testy
      zadanie4_tests()

      // Zadanie 5 Testy
      zadanie5_tests()
    }

  def zadanie1_tests(): Unit =
    {
      println("Zadanie 1 - Testy")
      var tree = generateTree(3, 50, 100)
      printTree(tree)
      println()
      tree = generateTree(4, 10, 20)
      printTree(tree)
      println()
      tree = generateTree(1, 5, 15)
      printTree(tree)
      println()
      tree = generateTree(0, 0, 5)
      printTree(tree)
      println()
    }

  def zadanie2_tests(): Unit =
    {
      println("Zadanie 2 - Testy")
      zadanie2_test1()
      zadanie2_test2()
    }

  def zadanie2_test1(): Unit =
    {
      println()
      println("Test 1")
      println()
      val tree1 = generateTree(2, 0, 10)
      val tree2 = generateTree(2, 0, 10)
      val tree3 = generateDifferTree(tree1, tree2)

      println("Drzewo 1:")
      printTree(tree1)
      println()

      println("Drzewo 2:")
      printTree(tree2)
      println()

      println("Drzewo 3:")
      printTree(tree3)
      println()
    }

  def zadanie2_test2(): Unit =
  {
    println()
    println("Test 2")
    println()
    val tree1 = generateTree(3, 0, 10)
    val tree2 = generateTree(3, 0, 10)
    val tree3 = generateDifferTree(tree1, tree2)

    println("Drzewo 1:")
    printTree(tree1)
    println()

    println("Drzewo 2:")
    printTree(tree2)
    println()

    println("Drzewo 3:")
    printTree(tree3)
    println()
  }

  def zadanie3_tests(): Unit =
    {
      println()
      println("Zadanie 3 - Testy")
      println()
      val tree1 = generateTree(3, 0, 2)
      val tree2 = generateTree(3, 0, 2)

      println("Drzewo 1:")
      printTree(tree1)
      println()

      println("Drzewo 2:")
      printTree(tree2)
      println()

      generateNoDuplicatesTreeDFS(tree1, tree2)

      println("Zmienione Drzewo 1: ")
      printTree(tree1)
      println()

      println("Zmienione Drzewo 2:")
      printTree(tree2)
      println()

      /*
      Np.:
      Drzewo 1:
      0 0 1 1 1 1 0 1 0 0 1 0 0 1 0
      Drzewo 2:
      1 0 0 0 1 1 0 1 0 1 0 0 1 1 1
      Zmienione Drzewo 1:
      -1 -1 -1 1 -1 -1 -1 0 1 0 0
      Zmienione Drzewo 2:
      -1 -1 -1 0 -1 -1 -1 1 0 1 1
       */
    }

  def zadanie4_tests(): Unit =
    {
      println()
      println("Zadanie 4 - Testy")
      println()
      var list = LazyList(5, 6, 3, 2, 1)
      var list2 = eachNElement(2, 3, list)
      println(list2.force == LazyList(5, 3))

      println()
      list = LazyList(5, 6, 3, 2, 1)
      list2 = eachNElement(2, 4, list)
      println(list2.force == LazyList(5, 3))

      println()
      list = LazyList.range(1, 10, 1)
      list2 = eachNElement(1, 7, list)
      println(list2.force == LazyList(1, 2, 3, 4, 5, 6, 7))

      println()
      list = LazyList.range(1, 20, 1)
      list2 = eachNElement(3, 18, list)
      println(list2.force == LazyList(1, 4, 7, 10, 13, 16))
    }

  def zadanie5_tests(): Unit =
    {
      println()
      println("Zadanie 5 - Testy")
      println()
      var list1 = LazyList(1, 2, 3)
      var list2 = LazyList(2, 3, 4, 5)
      var list = ldzialanie(list1, list2, (x: Int, y: Int) => x + y)
      println(list.force == LazyList(3, 5, 7, 5))

      println()
      list1 = LazyList(1, 2, 3, 8, 2, 1)
      list2 = LazyList(2, 3, 4, 5)
      list = ldzialanie(list1, list2, (x: Int, y: Int) => x - y)
      println(list.force == LazyList(-1, -1, -1, 3, 2, 1))

      println()
      list1 = LazyList(2, 3, 1, 2, 3)
      list2 = LazyList(2, 3, 4, 5, 0)
      list = ldzialanie(list1, list2, (x: Int, y: Int) => x * y)
      println(list.force == LazyList(4, 9, 4, 10, 0))

      println()
      val list3 = LazyList(10.0, 2.0, 4.0, 8.0, 2.0, 0.0, 0.0)
      val list4 = LazyList(2.0, 3.0, 4.0, 5.0, 0.0, 1.0)
      val lista = ldzialanie [Double](list3, list4, (x: Double, y: Double) => if (y != 0) x / y else 0)
      println(lista.force)
    }

  def printTree (tree: BT[Int]): Unit =
  {
    @scala.annotation.tailrec
    def breath(queue: List[BT[Int]]): Unit =
      queue match
      {
        case Nil =>
        case Node (v, left, right) :: t =>
          print(v + " ")
          breath (t ::: List(left, right))
        case Empty() :: t => breath (t)
        case _ =>
      }
    breath(List(tree))
  }
}
