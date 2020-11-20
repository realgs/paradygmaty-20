object trees {

  sealed trait BT[+A]

  case object Empty extends BT[Nothing]

  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  //pomocnicze
  def treeHeight[A](tree: BT[A]): Int = {
    tree match {
      case Empty => 0
      case Node(_, left, right) =>
        val leftHeight = treeHeight(left)
        val rightHeight = treeHeight(right)
        if (leftHeight > rightHeight) return leftHeight + 1
        else return rightHeight + 1
    }
  }

  def isFull[A](tree: BT[A]): Boolean = {
    def inner(node: BT[A]): Boolean = {
      node match {
        case Node(_, Empty, Empty) => true
        case Node(_, left, right) => treeHeight(left) == treeHeight(right) && isFull(left) && isFull(right)
        case _ => false
      }
    }

    if (tree == Empty) true
    else inner(tree)
  }

  def isInBound(tree: BT[Int], lower: Int, upper: Int): Boolean = {
    if (lower < 0 || lower > upper) throw new Exception("invalid argument")
    tree match {
      case Empty => true
      case Node(value, left, right) =>
        return value >= lower && value <= upper && isInBound(left, lower, upper) && isInBound(right, lower, upper)
      case _ => false
    }
  }

  //zadanie1 (3pkt)
  def fullTree(height: Int, lower: Int, upper: Int): BT[Int] = {
    val r = new scala.util.Random()

    def helper(h: Int): BT[Int] = {
      h match {
        case 0 => Empty;
        case _ => Node(r.between(lower, upper), helper(h - 1), helper(h - 1));
      }
    }

    if (lower < 0 || lower > upper) throw new Exception("invalid argument")
    else if (height < 0) throw new Exception("invalid argument")
    else helper(height)
  }

  //zadanie2 (3pkt)
  def treeSubtraction(t1: BT[Int], t2: BT[Int]): BT[Int] = {
    (t1, t2) match {
      case (Empty, Empty) => Empty
      case (_, Empty) | (Empty, _) => throw new Exception("Wrong input.Trees have different height")
      case (Node(v1, l1, r1), Node(v2, l2, r2)) => Node((v1 - v2), treeSubtraction(l1, l2), treeSubtraction(r1, r2))
    }
  }

  //zadanie3 (4pkt)
  //DFS
  def removeRepetitionDFS(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) = {
    if (!isFull(tree1) || !isFull(tree2)) throw new Exception("Wrong input. Not full trees")
    (tree1, tree2) match {
      case (Empty, Empty) => (Empty, Empty)
      case (Empty, _) | (_, Empty) => throw new Exception("Wrong input. Trees have different height")
      case (Node(v1, leftTree1, rightTree1), Node(v2, leftTree2, rightTree2)) =>
        val (left1, left2) = removeRepetitionDFS(leftTree1, leftTree2)
        val (right1, right2) = removeRepetitionDFS(rightTree1, rightTree2)
        if (v1 == v2) {
          if (left1 == Empty && right2 == Empty) return (Empty, Empty)
          else return (Node(-1, left1, right1), Node(-1, left2, right2))
        }
        else return (Node(v1, left1, right1), Node(v2, left2, right2))
    }
  }

  //BFS
  def removeRepetitionBFS(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) = {
    if (!isFull(tree1) || !isFull(tree2)) throw new Exception("Wrong input. Not full trees")
    (tree1, tree2) match {
      case (Empty, Empty) => (Empty, Empty)
      case (Empty, _) | (_, Empty) => throw new Exception("Wrong input.Trees have different height")
      case (Node(v1, leftTree1, rightTree1), Node(v2, leftTree2, rightTree2)) =>
        val (left1, left2) =
          if (areTreesEqualBreath(leftTree1, leftTree2)) (Empty, Empty)
          else removeRepetitionDFS(leftTree1, leftTree2)
        val (right1, right2) =
          if (areTreesEqualBreath(rightTree1, rightTree2)) (Empty, Empty)
          else removeRepetitionDFS(rightTree1, rightTree2)

        if (v1 == v2) {
          if (left1 == Empty & right2 == Empty) return (Empty, Empty)
          else return (Node(-1, left1, right1), Node(-1, left2, right2))
        }
        else return (Node(v1, left1, right1), Node(v2, left2, right2))
    }
  }

  def areTreesEqualBreath(left: BT[Int], right: BT[Int]): Boolean = {
    @scala.annotation.tailrec
    def traversal(queue: List[(BT[Int], BT[Int])]): Boolean =
      queue match {
        case Nil => true
        case (Empty, Empty) :: tail => traversal(tail)
        case (Node(v1, left1, right1), Node(v2, left2, right2)) :: tail =>
          if (v1 == v2) traversal(tail ++ List((left1, right1), (left2, right2)))
          else false
        case _ => false
      }

    traversal(List((left, right)))
  }
}

