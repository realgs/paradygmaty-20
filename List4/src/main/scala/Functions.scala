import scala.util.Random

object Functions {
  Random.setSeed(0)

  // Task 1
  def generateTree(depth: Int, valueMin: Int, valueMax: Int): BTree[Int] = {
    if (depth == 0) Empty
    else {
      BTree(Random.between(valueMin, valueMax), generateTree(depth - 1, valueMin, valueMax),
        generateTree(depth - 1, valueMin, valueMax))
    }
  }

  def intRootDiff(lhs: BTree[Int], rhs: BTree[Int]): Option[Int] = {
    (lhs.rootOption, rhs.rootOption) match {
      case (Some(x), Some(y)) => Some(x - y)
      case _ => None
    }
  }

  // Task 2
  def elementwiseDiff(t1: BTree[Int], t2: BTree[Int])(rootDiff: (BTree[Int], BTree[Int]) => Option[Int]): BTree[Int] = {
    if (rootDiff(t1, t2).isEmpty) Empty
    else {
      BTree(rootDiff(t1, t2).get, elementwiseDiff(t1.getLeft.getOrElse(Empty), t2.getLeft.getOrElse(Empty))(rootDiff),
        elementwiseDiff(t1.getRight.getOrElse(Empty), t2.getRight.getOrElse(Empty))(rootDiff))
    }
  }
}
