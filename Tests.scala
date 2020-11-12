import scala.reflect.internal.Depth

class Functions {

  sealed trait BT[A]

  case class Leaf[A]() extends BT[A]

  case class Node[A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  //zadanie 1 (3pkt)
  def generateTree(depth: Int, x: Int, y: Int): BT[Int] = {
    if (depth < 1) throw new Exception("Depth must be higher than 0!")
    else if(y>x) throw new Exception("Incorrect range!")
    else getNode(depth, x, y)
  }

  private def getNode(depth: Int, x: Int, y: Int): BT[Int] = {
    depth match {
      case 0 => Leaf()
      case i => Node(i, getNode(i-1), getNode(i-1))
    }
  }
}
