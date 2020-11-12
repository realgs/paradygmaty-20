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
  
  def elementwiseDifference[A](t1: BTree[A], t2: BTree[A])(rootDiff: (BTree[A], BTree[A]) => A): BTree[A] = {
    BTree(rootDiff(t1, t2),
      elementwiseDifference(???, ???)(rootDiff),
      elementwiseDifference(???, ???)(rootDiff))
  }
}
