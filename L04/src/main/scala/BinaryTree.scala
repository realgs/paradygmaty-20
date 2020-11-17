trait BinaryTree[+A]
case object Empty extends BinaryTree[Nothing]
case class Node[+A](el: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]
