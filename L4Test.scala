import org.scalatest.FunSuite

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A,left:BT[A],right:BT[A])extends BT[A]

class L4Test extends FunSuite {
  val tested = new L4

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

  test("GenerateFullTreeTest"){
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
}
