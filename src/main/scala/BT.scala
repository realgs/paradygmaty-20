sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](element: A, leftBT: BT[A], rightBT: BT[A]) extends BT[A]
