import l4_2.{Empty, Node, BT}
object l4_tree_examples {
  val tree1: BT[Int] = Node(1, Node(1, Node(1, Empty, Empty), Node(1, Empty, Empty)), Node(1, Node(1, Empty, Empty), Node(1, Empty, Empty)))
  val tree2: BT[Int] = Node(4, Node(4, Node(1, Empty, Empty), Node(4, Empty, Empty)), Node(1, Node(1, Empty, Empty), Node(1, Empty, Empty)))
  val tree3: BT[Int] = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))
  val tree4: BT[Int] = Node(1, Node(2, Empty, Empty), Node(1, Empty, Empty))
  val tree5: BT[Int] = Node(1, Node(2, Node(3, Empty, Empty), Node(4, Empty, Empty)), Node(5, Node(6, Empty, Empty), Node(7, Empty, Empty)))
  val tree6: BT[Int] = Node(1, Node(2, Node(99, Empty, Empty), Node(4, Empty, Empty)), Node(99, Node(6, Empty, Empty), Node(7, Empty, Empty)))


  val treeResult1: BT[Int] = Node(0, Node(0, Node(0, Empty, Empty), Node(0, Empty, Empty)), Node(0, Node(0, Empty, Empty), Node(0, Empty, Empty)))
  val treeResult2: BT[Int] = Node(-3, Node(-3, Node(0, Empty, Empty), Node(-3, Empty, Empty)), Node(0, Node(0, Empty, Empty), Node(0, Empty, Empty)))
  val treeResult3: BT[Int] = Node(-1, Empty, Node(3, Empty, Empty))
  val treeResult4: BT[Int] = Node(-1, Empty, Node(1, Empty, Empty))
  val treeResult5: BT[Int] = Node(-1, Node(-1, Node(3, Empty, Empty), Empty), Node(5, Empty, Empty))
  val treeResult6: BT[Int] = Node(-1, Node(-1, Node(99, Empty, Empty), Empty), Node(99, Empty, Empty))
  val treeResult7: BT[Int] = Empty
}

