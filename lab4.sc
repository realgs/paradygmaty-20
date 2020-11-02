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

generateTree(0,(10,20))
generateTree(1,(10,20))
generateTree(2,(10,20))
generateTree(3,(10,20))

// 2) - 3pkt

// 3) - 4pkt

// 4) - 5pkt

// 5) - 5pkt