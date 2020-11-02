// Karol Waliszewski

// Binary tree
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

// Random positive number
def getRandomNumber(range:(Int, Int)):Int = {
  if(range._1 <= 0 || range._2 <= 0)
    throw new Exception("Range have to be positive value.")

  if(range._2 < range._1)
    throw new Exception("Invalid range.")

  val rand = scala.util.Random
  rand.nextInt(range._2 - range._1 + 1) + range._1
}

// 1) - 3pkt
def generateTree(depth: Int, range:(Int, Int)):BT[Int] = {
  if(depth <= 0)
    Empty
  else if(depth == 1)
    Node(getRandomNumber(range), Empty, Empty)
  else
    Node(getRandomNumber(range), generateTree(depth - 1, range), generateTree(depth - 1, range))
}

generateTree(-1, (0, 0))
generateTree(0, (10, 20))
generateTree(1, (10, 20))
generateTree(2, (10, 20))
generateTree(3, (10, 20))

// 2) - 3pkt
def subtractTrees(tree1: BT[Int], tree2: BT[Int]):BT[Int] =
  (tree1, tree2) match {
    case (Node(t1, t1l, t1r), Node(t2, t2l, t2r)) => Node(t1 - t2, subtractTrees(t1l, t2l), subtractTrees(t1r, t2r))
    case (_, _) => Empty
  }

val tree1 = generateTree(3, (10, 50))
val tree2 = generateTree(3, (40, 100))

subtractTrees(tree1, tree2)

// 3) - 4pkt

// 4) - 5pkt

// 5) - 5pkt