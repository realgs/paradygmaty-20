import scala.util.Random

object Lista4 extends App {


  // zadanie 1 (3 punkty)
  def generateRandomTree(depth: Int, lowerNumberBound: Int, upperNumberBound: Int): BT[Int] = {
    def helper(actualDepth: Int): BT[Int] = {
      if (actualDepth == depth) Empty
      else Node(Random.between(lowerNumberBound, upperNumberBound + 1), helper(actualDepth + 1), helper(actualDepth + 1))
    }

    if (lowerNumberBound <= 0 || upperNumberBound <= 0) throw new IllegalArgumentException("Non positive numbers are not allowed")
    if (lowerNumberBound > upperNumberBound) throw new IllegalArgumentException("Lower bound cannot be grater than upper")

    if (depth == 0) Empty
    else if (depth == 1) Node(Random.between(lowerNumberBound, upperNumberBound + 1), Empty, Empty)
    else helper(0)
  }

  // zadanie 2 (3 punkty)
  // pytanie - czy w srodku funkcji musimy zadbac o to ze drzewa na wejsciu sa z funkcji z zadania 1
  def treesDifference(tree1: BT[Int], tree2: BT[Int]): BT[Int] = {
    def helper(tree1: BT[Int], tree2: BT[Int]): BT[Int] = (tree1, tree2) match {
      case (Empty, Empty) => Empty
      case (Node(value1, left1, right1), Node(value2, left2, right2)) => Node(value1 - value2, treesDifference(left1, left2), treesDifference(right1, right2))
    }

    if(Utils.calculateDepth(tree1) != Utils.calculateDepth(tree2)) throw new IllegalArgumentException("Trees must have same depths")
    else helper(tree1, tree2)
  }


}
