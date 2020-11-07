//Jakub Kochański
//Zad1 (3pkt)
sealed trait BinaryTree[+A]
case object Empty extends BinaryTree[Nothing]
case class Node[+A](value:A,left:BinaryTree[A],right:BinaryTree[A]) extends BinaryTree[A]

val generateTree: Int => (Int,Int) => BinaryTree[Int] = depth => (from,to) =>
  {
    def generateValue(from:Int,to:Int):Int = from + scala.util.Random.nextInt(to - from + 1)

    def generateNode(value:Int,currentDepth:Int):BinaryTree[Int] =
      {
        if(currentDepth == depth) Empty
        else Node(value,generateNode(generateValue(from,to),currentDepth + 1),generateNode(generateValue(from,to),currentDepth + 1))
      }
    if(depth < 0 || from > to || from < 0) throw new IllegalArgumentException("generateTree")
    generateNode(generateValue(from,to),0)
  }

val testTree: BinaryTree[Int] => (Int,Int) => Int => Boolean = treeToTest => (min,max) => depth => {
  def countNodes(binaryTree: BinaryTree[Int]):Int =
    {
      binaryTree match {
        case Empty => 0
        case Node(value,left,right) =>
          assert(value >= min && value <= max,"Illegal node value!")
          1 + countNodes(left) + countNodes(right)
      }
    }

  countNodes(treeToTest) == scala.math.pow(2,depth) - 1
}

testTree(generateTree(3)(0,5))(0,5)(3)
testTree(generateTree(8)(0,100))(0,100)(8)
testTree(generateTree(15)(100,400))(100,400)(15)