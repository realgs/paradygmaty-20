package scala.trees

trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](value: A, left: BT[A], right: BT[A]) extends BT[A]
