package List6

import _root_.List6.SearchMaxTree.{BT, Empty, Node}
import scala.annotation.tailrec

object TreesToTest {

  //method for generating tree to test for bigger numbers
  @tailrec
  def createTree(depth: Int, rangeFrom: Int, rangeTo: Int): BT[Int] = {
    if (depth < 0) throw new Exception("Invalid data of depth")
    else {
      if (rangeFrom > rangeTo) createTree(depth, rangeTo, rangeFrom)
      else if (rangeFrom < 0 && rangeTo < 0) throw new Exception("The range have to start with positive number")
      else if (rangeFrom < 0) createTree(depth, 0, rangeTo)
      else {
        def createTreeIter(currentDepth: Int): BT[Int] = {
          if (currentDepth == depth) Empty
          else {
            Node(randomisation(rangeFrom, rangeTo), createTreeIter(currentDepth + 1), createTreeIter(currentDepth + 1))
          }
        }

        createTreeIter(0)
      }
    }
  }

  //helpful method to random numbers
  def randomisation(from: Int, to: Int): Int = {
    val r = scala.util.Random
    r.nextInt(to + 1) + from
  }

  val treeInput1: BT[Int] = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))
  val treeInput2: BT[Int] = Node(1, Node(4, Empty, Empty), Node(3, Empty, Empty))
  val treeInput3: BT[Int] = Node(1, Node(2, Node(4, Node(6, Empty, Empty), Node(7, Empty, Empty)), Node(5, Node(8, Empty, Empty), Node(9, Empty, Empty))), Node(3, Node(10, Node(120, Empty, Empty), Node(13, Empty, Empty)), Node(11, Node(14, Empty, Empty), Node(15, Empty, Empty))))
  val treeInput4: BT[Int] = Node(1, Node(2, Node(4, Node(6, Empty, Empty), Node(7, Empty, Empty)), Node(5, Node(8, Empty, Empty), Node(90, Empty, Empty))), Node(3, Node(9, Node(12, Empty, Empty), Node(13, Empty, Empty)), Node(11, Node(13, Empty, Empty), Node(17, Empty, Empty))))
  val treeInput5: BT[Int] = Node(-1, Node(-1, Node(-5,Empty, Empty), Node(-27, Empty, Empty)), Node(-6, Node(-18,Empty, Empty), Node(-1, Empty, Empty)))
  val treeInput6: BT[Int] = Node(80, Node(9, Node(6,Empty, Empty), Node(27, Empty, Empty)), Node(10, Node(19,Empty, Empty), Node(0, Empty, Empty)))
  val treeInput7: BT[Int] = Node(1, Empty, Empty)
  val treeInput8 = Empty

  val max1 = 3
  val max2 = 4
  val max3 = 120
  val max4 = 90
  val max5 = -1
  val max6 = 80
  val max7 = 1
  val max8 = Int.MinValue

}
