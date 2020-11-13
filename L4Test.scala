import org.scalatest.FunSuite

import scala.annotation.tailrec

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A,left:BT[A],right:BT[A])extends BT[A]

class L4Test extends FunSuite {
  val tested = new L4

  //funkcje przydatne do testowania
  def treeToList[A](tree:BT[A]):List[A] ={
    def helper(treeList:List[BT[A]]):List[A] =
      treeList match{
        case Nil =>Nil
        case Empty::tail => helper(tail)
        case Node(value,leftTree,rightTree)::tail =>
          value::helper(tail:::List(leftTree,rightTree))
      }
    helper(List(tree))
  }

  def depthOfTree(tree:BT[Int]):Int = {
    def helper(tree: BT[Int], depth: Int): Int =
      tree match {
        case Node(_, left, right) =>
          val a = helper(left, depth + 1)
          val b = helper(right, depth + 1)
          if (a > b) a
          else b
        case Empty => depth
      }
      helper(tree,0)
  }

  def numberOfNodes(tree:BT[Int]):Int =
    tree match{
      case Node(_,left,right) => 1+numberOfNodes(left)+numberOfNodes(right)
      case Empty => 0
    }

  def isInInterval(tree:BT[Int],x:Int,y:Int):Boolean ={
    def helper(tree:BT[Int]):Boolean =
      tree match {
        case Node(value, left, right) =>
          if (value >= x && value <= y) helper(left) && helper(right)
          else false
        case Empty => true
      }
    helper(tree)
  }

  def isFullTree(tree:BT[Int]):Boolean ={
    Math.pow(2,depthOfTree(tree))-1 == numberOfNodes(tree)
  }

  // testy
  test("Generate Full Tree Test"){
    val tree1 = tested.generateTree(15,40,120)
    assert(depthOfTree(tree1)==15)
    isInInterval(tree1,40,120)
    assert(isFullTree(tree1))

    val tree2 = tested.generateTree(10,0,1)
    assert(depthOfTree(tree2)==10)
    isInInterval(tree2,0,1)
    assert(isFullTree(tree2))

    val tree3 = tested.generateTree(1,0,1)
    assert(depthOfTree(tree3)==1)
    isInInterval(tree3,0,1)
    assert(isFullTree(tree3))
  }

  test("Subtraction of Trees Test"){
    val tree1 = tested.generateTree(11,5,20)
    val tree1List = treeToList(tree1)
    val tree2 = tested.generateTree(11, 0,10)
    val tree2List = treeToList(tree2)

    @tailrec
    def check(xs1:List[Int], xs2:List[Int], xs3:List[Int]):Boolean ={
      if(xs3==Nil) true
      else if(xs1.head - xs2.head == xs3.head) check(xs1.tail,xs2.tail,xs3.tail)
      else false
    }
    val tree3 = tested.subOfTrees(tree1,tree2)
    val tree3List = treeToList(tree3)

    assert(check(tree1List,tree2List,tree3List))
  }
}
