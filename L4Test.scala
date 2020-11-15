import org.scalatest.FunSuite

import scala.annotation.tailrec

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A,left:BT[A],right:BT[A])extends BT[A]

sealed trait nlist[+A]
case class Koniec[+A]() extends nlist[A]
case class Element[+A](elem: A,list: nlist[A]) extends nlist[A]

sealed  trait llist[+A]
case class LKoniec[+A]() extends llist[A]
case class LElement[+A](elem: A,tail: ()=>llist[A]) extends llist[A]

class L4Test extends FunSuite {
  val tested = new L4

  //funkcje przydatne do testowania
  def treeToList[A](tree: BT[A]): List[A] = {
    def helper(treeList: List[BT[A]]): List[A] =
      treeList match {
        case Nil => Nil
        case Empty :: tail => helper(tail)
        case Node(value, leftTree, rightTree) :: tail =>
          value :: helper(tail ::: List(leftTree, rightTree))
      }

    helper(List(tree))
  }

  def depthOfTree(tree: BT[Int]): Int = {
    def helper(tree: BT[Int], depth: Int): Int =
      tree match {
        case Node(_, left, right) =>
          val a = helper(left, depth + 1)
          val b = helper(right, depth + 1)
          if (a > b) a
          else b
        case Empty => depth
      }

    helper(tree, 0)
  }

  def numberOfNodes(tree: BT[Int]): Int =
    tree match {
      case Node(_, left, right) => 1 + numberOfNodes(left) + numberOfNodes(right)
      case Empty => 0
    }

  def isInInterval(tree: BT[Int], x: Int, y: Int): Boolean = {
    def helper(tree: BT[Int]): Boolean =
      tree match {
        case Node(value, left, right) =>
          if (value >= x && value <= y) helper(left) && helper(right)
          else false
        case Empty => true
      }

    helper(tree)
  }

  def isFullTree(tree: BT[Int]): Boolean = {
    Math.pow(2, depthOfTree(tree)) - 1 == numberOfNodes(tree)
  }

  def nlistTollist(list: nlist[Int]): llist[Int] =
    list match {
      case Element(elem, tail) => LElement(elem, () => nlistTollist(tail))
      case Koniec() => LKoniec()
    }

  def llistTonlist(lList: llist[Int]): nlist[Int] =
    lList match {
      case LElement(elem, tail) => Element(elem,llistTonlist(tail()))
      case LKoniec() => Koniec()
    }

  // testy
  test("Generate Full Tree Test") {
    val tree1 = tested.generateTree(15, 40, 120)
    assert(depthOfTree(tree1) == 15)
    isInInterval(tree1, 40, 120)
    assert(isFullTree(tree1))

    val tree2 = tested.generateTree(10, 0, 1)
    assert(depthOfTree(tree2) == 10)
    isInInterval(tree2, 0, 1)
    assert(isFullTree(tree2))

    val tree3 = tested.generateTree(1, 0, 1)
    assert(depthOfTree(tree3) == 1)
    isInInterval(tree3, 0, 1)
    assert(isFullTree(tree3))
  }

  test("Subtraction of Trees Test") {
    val tree1 = tested.generateTree(11, 5, 20)
    val tree1List = treeToList(tree1)
    val tree2 = tested.generateTree(11, 0, 10)
    val tree2List = treeToList(tree2)

    @tailrec
    def check(xs1: List[Int], xs2: List[Int], xs3: List[Int]): Boolean = {
      if (xs3 == Nil) true
      else if (xs1.head - xs2.head == xs3.head) check(xs1.tail, xs2.tail, xs3.tail)
      else false
    }

    val tree3 = tested.subOfTrees(tree1, tree2)
    val tree3List = treeToList(tree3)

    assert(check(tree1List, tree2List, tree3List))
  }

  test("Delete Same Elements Depth Test") {
    //test1 (głębokość 2)
    val tree1 = Node(1, Node(2, Empty, Empty), Node(4, Empty, Empty))
    val tree2 = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))

    assert(tested.sameElemDelDepth(tree1, tree2) == (Node(-1, Empty, Node(4, Empty, Empty)),
      Node(-1, Empty, Node(3, Empty, Empty))))

    //test2 (głębokość 3)
    val tree3 = Node(1, Node(2, Node(3, Empty, Empty), Node(4, Empty, Empty)), Node(5, Node(6, Empty, Empty), Node(7, Empty, Empty)))
    val tree4 = Node(10, Node(2, Node(30, Empty, Empty), Node(40, Empty, Empty)), Node(5, Node(60, Empty, Empty), Node(7, Empty, Empty)))

    assert(tested.sameElemDelDepth(tree3,tree4)== (
      Node(1, Node(-1, Node(3, Empty, Empty), Node(4, Empty, Empty)), Node(-1, Node(6, Empty, Empty), Empty)),
      Node(10, Node(-1, Node(30, Empty, Empty), Node(40, Empty, Empty)), Node(-1, Node(60, Empty, Empty), Empty))
      ))

  }

  test("Each N Element Test") {
    val test = Element(5,Element(6,Element(3,Element(2,Element(1,Koniec())))))

    val expected1 = Element(5,Element(3,Koniec()))
    val expected2 = Element(5,Element(3,Element(1,Koniec())))
    val expected3 = Element(5,Element(6,Element(3,Koniec())))
    val expected4 = Element(5,Element(6,Element(3,Element(2,Element(1,Koniec())))))
    val expected5 = Koniec()

    val result1 = tested.eachNElement(nlistTollist(test),2,3)
    val result2 = tested.eachNElement(nlistTollist(test),2,5)
    val result3 = tested.eachNElement(nlistTollist(test),1,3)
    val result4 = tested.eachNElement(nlistTollist(test),1,5)
    val result5 = tested.eachNElement(nlistTollist(test),1,0)

    assert(llistTonlist(result1)==expected1)
    assert(llistTonlist(result2)==expected2)
    assert(llistTonlist(result3)==expected3)
    assert(llistTonlist(result4)==expected4)
    assert(llistTonlist(result5)==expected5)
  }

  test("ldzialanie Test"){
    val list1 = Element(1,Element(2,Element(3,Koniec())))
    val list2 = Element(2,Element(3,Element(4,Element(5,Koniec()))))
    val sign1 ='+'
    val expected1 = Element(3,Element(5,Element(7,Element(5,Koniec()))))

    val list3 = Element(3,Element(4,Element(5,Element(10,Koniec()))))
    val list4 = Element(10,Element(2,Koniec()))
    val sign2 ='*'
    val expected2 = Element(30,Element(8,Element(5,Element(10,Koniec()))))

    val list5 = Element(10,Element(15,Element(8,Element(12,Koniec()))))
    val list6 = Element(2,Element(5,Element(8,Element(3,Koniec()))))
    val sign3 ='/'
    val expected3 = Element(5,Element(3,Element(1,Element(4,Koniec()))))

    val list7 = Element(10,Element(15,Element(8,Element(12,Koniec()))))
    val list8 = Element(2,Element(5,Element(8,Element(3,Koniec()))))
    val sign4 ='-'
    val expected4 = Element(8,Element(10,Element(0,Element(9,Koniec()))))

    val list9 = Koniec()
    val list10 = Koniec()
    val sign5 ='+'
    val expected5 = Koniec()

    val result1 = tested.ldzialanie(nlistTollist(list1),nlistTollist(list2),sign1)
    val result2 = tested.ldzialanie(nlistTollist(list3),nlistTollist(list4),sign2)
    val result3 = tested.ldzialanie(nlistTollist(list5),nlistTollist(list6),sign3)
    val result4 = tested.ldzialanie(nlistTollist(list7),nlistTollist(list8),sign4)
    val result5 = tested.ldzialanie(nlistTollist(list9),nlistTollist(list10),sign5)

    assert(llistTonlist(result1)==expected1)
    assert(llistTonlist(result2)==expected2)
    assert(llistTonlist(result3)==expected3)
    assert(llistTonlist(result4)==expected4)
    assert(llistTonlist(result5)==expected5)
  }
}
