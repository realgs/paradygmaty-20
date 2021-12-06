import l4_2.{*, +, -, /, createTree, eachNElement, ldzialanie, printBT, removeDuplicatesBFS, removeDuplicatesDFS, subtract}
import l4_tree_examples.{tree1, tree2, tree3, tree4, tree5, tree6, treeResult1, treeResult2, treeResult3, treeResult4, treeResult5, treeResult6, treeResult7}

object l4_2_tests {
  def main(args: Array[String]): Unit = {

    println("Zadanie 1:")
    printBT(createTree(2, 0, 10))
    printBT(createTree(4, 10, 100))
    println("Zadanie 2:")
    println(subtract(tree1, tree1) == treeResult1)
    println(subtract(tree6, tree6) == treeResult1)
    println(subtract(tree1, tree2) == treeResult2)
    println("Zadanie 2 DFS:")
    println(removeDuplicatesDFS(tree3, tree4) == (treeResult3, treeResult4))
    println(removeDuplicatesDFS(tree5, tree6) == (treeResult5, treeResult6))
    println(removeDuplicatesDFS(tree1, tree1) == (treeResult7, treeResult7))
    println("Zadanie 3 BFS:")
    println(removeDuplicatesBFS(tree3, tree4) == (treeResult3, treeResult4))
    println(removeDuplicatesBFS(tree5, tree6) == (treeResult5, treeResult6))
    println(removeDuplicatesBFS(tree1, tree1) == (treeResult7, treeResult7))
    println("Zadanie 4:")
    println(eachNElement(LazyList(5,6,3,2,1), 2, 3) == LazyList(5,3))
    println(eachNElement(LazyList(5,6,3,2,1), 2, 4) == LazyList(5,3))
    println(eachNElement(LazyList(1,2,3,4,5,6,7,8), 1, 60) == LazyList(1,2,3,4,5,6,7,8))
    println(eachNElement(LazyList('a','b','c','d','e','f','g'), 3,6) == LazyList('a','d'))
    println(eachNElement(LazyList(), 2, 3) == LazyList())
    println(eachNElement(LazyList(1,1,1), 32, 0) == LazyList())
    println("Zadanie 5:")
    println(ldzialanie(LazyList(1,2,3), LazyList(2,3,4,5), +) == LazyList(3, 5, 7, 5))
    println(ldzialanie(LazyList(), LazyList(1,2,3,4), +) == LazyList(1, 2, 3, 4))
    println(ldzialanie(LazyList(1,2,3,4), LazyList(), +) == LazyList(1, 2, 3, 4))
    println(ldzialanie(LazyList(1,1,1,1), LazyList(1,2,3), -) == LazyList(0, -1, -2, 1))
    println(ldzialanie(LazyList(), LazyList(), -) == LazyList())
    println(ldzialanie(LazyList(1,1,1,1,1), LazyList(2,3,4), *) == LazyList(2, 3, 4, 1, 1))
    println(ldzialanie(LazyList(1,1,1), LazyList(2,3,4,5,6,7), *) == LazyList(2, 3, 4, 5, 6, 7))
    println(ldzialanie(LazyList(24,48,5,50,51), LazyList(6,6,4), /) == LazyList(4, 8, 1.25, 50, 51))
    println(ldzialanie(LazyList(), LazyList(), /) == LazyList())
  }
}

