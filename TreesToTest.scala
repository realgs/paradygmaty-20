package List4

import _root_.List4.L4Trees.Empty
import _root_.List4.L4Trees.Node
import _root_.List4.L4Trees.BT

object TreesToTest {

  val treeInput1: BT[Int] = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))
  val treeInput2: BT[Int] = Node(1, Node(4, Empty, Empty), Node(3, Empty, Empty))
  val treeInput3: BT[Int] = Node(1, Node(2, Node(4, Node(6, Empty, Empty), Node(7, Empty, Empty)), Node(5, Node(8, Empty, Empty), Node(9, Empty, Empty))), Node(3, Node(10, Node(12, Empty, Empty), Node(13, Empty, Empty)), Node(11, Node(14, Empty, Empty), Node(15, Empty, Empty))))
  val treeInput4: BT[Int] = Node(1, Node(2, Node(4, Node(6, Empty, Empty), Node(7, Empty, Empty)), Node(5, Node(8, Empty, Empty), Node(9, Empty, Empty))), Node(3, Node(9, Node(12, Empty, Empty), Node(13, Empty, Empty)), Node(11, Node(13, Empty, Empty), Node(17, Empty, Empty))))
  val treeInput5: BT[Int] = Node(1, Node(1, Node(-5,Empty, Empty), Node(-27, Empty, Empty)), Node(6, Node(18,Empty, Empty), Node(1, Empty, Empty)))
  val treeInput6: BT[Int] = Node(8, Node(9, Node(6,Empty, Empty), Node(27, Empty, Empty)), Node(10, Node(19,Empty, Empty), Node(0, Empty, Empty)))
  val treeInput7: BT[Int] = Node(1, Empty, Empty)
  val treeInput8 = treeInput7
  val treeInput9: BT[Int] = Node(1, Node(2, Empty, Empty), Empty)

  val treeOutput1: BT[Int] = Node(-1,Node(2,Empty,Empty),Empty)
  val treeOutput2: BT[Int] = Node(-1,Node(4,Empty,Empty),Empty)
  val treeOutput3: BT[Int] = Node(-1,Empty,Node(-1,Node(10,Empty,Empty),Node(-1,Node(14,Empty,Empty),Node(15,Empty,Empty))))
  val treeOutput4: BT[Int] = Node(-1,Empty,Node(-1,Node(9,Empty,Empty),Node(-1,Node(13,Empty,Empty),Node(17,Empty,Empty))))
  val treeOutput5 = treeInput5
  val treeOutput6 = treeInput6
  val treeOutput7 = (Empty, Empty)


}