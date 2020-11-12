import scala.util.Random

object Functions {
  Random.setSeed(0)

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

  def elementwiseDifference[A](t1: BTree[A], t2: BTree[A])(diff: Vertex[A] => Vertex[A] => Option[A]): Unit = {
    print(t1.rootOption)
    print(t2.rootOption)
  }
}
