object BinaryTreesTasks {
  //zadanie 1 - 3 punkty
  val r = scala.util.Random

  def gen(a: Int, b: Int): Int = a + r.nextInt((b - a) + 1)

  def tree_generate(deep: Int, a: Int, b: Int): BT[Int] = {
    if (deep < 0) throw new Exception("n < 0") else
    deep match {
      case 0 => Empty
      case 1 => Node(gen(a, b), Empty, Empty)
      case _ => Node(gen(a, b), tree_generate(deep - 1, a, b), tree_generate(deep - 1, a, b))
    }
  }

  //functions for testing tree
  def goThroughTree(nodeQue: List[BT[Int]]): List[Int] = { // wszerz
    nodeQue match {
      case Nil => Nil
      case Empty :: tail => goThroughTree(tail)
      case Node(value, left, right) :: tail => value :: goThroughTree(tail ++ List(left, right))
    }
  }

  def isFullTree(tree: BT[Int]): Boolean = {
    if (goThroughTree(List(tree)).size == scala.math.pow(2, checkDeep(tree)) - 1) true
    else false
  }

  def checkDeep(tree: BT[Int]): Int = {
    def checkDeepOneSide(t: BT[Int], isRight: Boolean): Int = {
      (t, isRight) match {
        case (Empty, _) => 0
        case (Node(_, _, right), true) => 1 + checkDeepOneSide(right, true)
        case (Node(_, left, _), false) => 1 + checkDeepOneSide(left, false)
      }
    }

    if (checkDeepOneSide(tree, true) > checkDeepOneSide(tree, false)) checkDeepOneSide(tree, true)
    else checkDeepOneSide(tree, false)
  }

  //zadanie 2 - 3 pkt
  def subtractTrees(t1: BT[Int], t2: BT[Int]):BT[Int] =
    (t1, t2) match {
      case (Node(value1, left1, right1), Node(value2, left2, right2)) => Node(value1 - value2, subtractTrees(left1, left2), subtractTrees(right1, right2))
      case (_, _) => Empty
    }

  //zadanie 3
  def sameSubTrees(tree1:BT[Int], tree2:BT[Int]): (BT[Int], BT[Int]) = {
    //wgłąb
    def sameElemDelDepth(t1: BT[Int], t2: BT[Int]): (BT[Int], BT[Int]) = {
      (t1, t2) match {
        case (Node(v1, l1, r1), Node(v2, l2, r2)) =>
          val (lt1, lt2) = sameElemDelDepth(l1, l2)
          val (rt1, rt2) = sameElemDelDepth(r1, r2)
          if (v1 == v2) {
            if (lt1 == Empty && rt1 == Empty) (Empty, Empty)
            else (Node(-1, lt1, rt1), Node(-1, lt2, rt2))
          }
          else (Node(v1, lt1, rt1), Node(v2, lt2, rt2))
        case (Empty, Empty) => (Empty, Empty)
        case (_, _) => throw new Exception("Incorrect Tree")
      }
    }
    sameElemDelDepth(tree1, tree2)
  }
}
