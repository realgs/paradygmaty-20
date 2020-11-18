
import Lista4._

object TestsForLista4 extends App {

  println("Start tests")
  val testTree1 = Node(2, Node(5, Node(4, Empty, Empty), Node(3, Empty, Empty)), Node(5, Node(5, Empty, Empty), Node(1, Empty, Empty)))
  val testTree2 = Node(1, Node(4, Node(2, Empty, Empty), Node(5, Empty, Empty)), Node(2, Node(4, Empty, Empty), Node(4, Empty, Empty)))
  val testTree3 = Node(3, Node(5, Empty, Empty), Node(3, Empty, Empty))
  val testTree4 = Node(2, Node(1, Empty, Empty), Node(2, Empty, Empty))
  val testTree5 = Node(2, Node(5, Node(4, Empty, Empty), Node(3, Empty, Empty)), Node(2, Node(1, Empty, Empty), Node(1, Empty, Empty)))
  val testTree6 = Node(3, Node(1, Empty, Empty), Node(3, Empty, Empty))

  println("\nZad 1: generate random trees with depth 0, 1, 2 and 3")
  println(generujDrzewo(0))
  println(generujDrzewo(1))
  println(generujDrzewo(2))
  println(generujDrzewo(3))
  println("\nZad 1: generating trees with depth <0 causes generation of 0 depth tree")
  println(generujDrzewo(-1))
  println(generujDrzewo(-20))
  println(generujDrzewo(0))

  println("\nZad 2")
  println(odejmijDrzewo(testTree3, testTree4) == Node(1, Node(4, Empty, Empty), Node(1, Empty, Empty)))
  println(odejmijDrzewo(testTree1, testTree2) == Node(1, Node(1, Node(2, Empty, Empty), Node(-2, Empty, Empty)), Node(3, Node(1, Empty, Empty), Node(-3, Empty, Empty))))
  try {
    odejmijDrzewo(testTree1, testTree3)
  } catch {
    case e: Exception => println("Planned exception captured: Drzewo 2 mniejsze od drzewa 1 lub drzewa nieregularne")
  }

  println("\nZad 3")
  println(removeInDepth(testTree1, testTree5)==(Node(-1,Empty,Node(5,Node(5,Empty,Empty),Empty)),Node(-1,Empty,Node(2,Node(1,Empty,Empty),Empty))))
  println(removeInDepth(testTree3, testTree6)==(Node(-1, Node(5, Empty, Empty), Empty),Node(-1, Node(1, Empty, Empty), Empty)))
  try {
    removeInDepth(testTree3, testTree1)
  } catch {
    case e: Exception => println("Planned exception captured: Drzewo 1 mniejsze od drzewa 2 lub drzewa nieregularne")
  }

  println("\nZad 4")
  println(nLazyListElemsToList(eachNElement(LazyList(), 2, 3), 20)==List())
  println(nLazyListElemsToList(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 3), 20)==List(1,3))
  println(nLazyListElemsToList(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 4), 20)==List(1,3))
  println(nLazyListElemsToList(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 7), 20)==List(1,4,7))
  println(nLazyListElemsToList(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 3, 20), 20)==List(1,4,7,10))

  println("\nZad 5")
  println(nLazyListElemsToList(ldzialanie(LazyList(1, 2, 3, 4), LazyList(1, 2, 3, 4), +), 20)==List(2,4,6,8))
  println(nLazyListElemsToList(ldzialanie(LazyList(1, 2, 3), LazyList(-3, -2, -1), +), 20)==List(-2,0,2))
  println(nLazyListElemsToList(ldzialanie(LazyList(1, 2, 3, 4), LazyList(1, 2, 3, 4), -), 20)==List(0,0,0,0))
  println(nLazyListElemsToList(ldzialanie(LazyList(1, 2, 3, 4), LazyList(1, 2, 3, 4), *), 20)==List(1,4,9,16))
  println(nLazyListElemsToList(ldzialanie(LazyList(1, 2, 3, 4), LazyList(1, 2, 3, 4), /), 20)==List(1,1,1,1))
  println(nLazyListElemsToList(ldzialanie(LazyList(1, 2, 3), LazyList(-3, -2, -1), *), 20)==List(-3,-4,-3))
  println(nLazyListElemsToList(ldzialanie(LazyList(1, 2, 3, 4), LazyList(1, 2, 3, 4, 5, 6), +), 20)==List(2,4,6,8,5,6))

}
