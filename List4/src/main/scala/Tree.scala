/*
sealed trait Tree[+A] {
  def rootOption: Option[A]
  def isEmpty: Boolean
}

case class NonEmptyTree[+A](root: A, left: Tree[A], right: Tree[A]) extends Tree[A] {
  def rootOption: Some[A] = Some(root)
  def isEmpty = false
}

object NonEmptyTree {
  def apply[A](root: A, left: Tree[A], right: Tree[A]): NonEmptyTree[A] = NonEmptyTree(root, left, right)
}

case object EmptyTree extends Tree[Nothing] {
  def rootOption: Option[Nothing] = None
  def isEmpty = true
}

 */

sealed trait BTree[+A] {
  def rootOption: Option[A]

  def isEmpty: Boolean
}

object BTree {
  def apply[A](): BTree[A] = Empty

  def apply[A](data: A, left: BTree[A], right: BTree[A]): BTree[A] = Vertex(data, left, right)
}

case class Vertex[A](data: A, left: BTree[A], right: BTree[A]) extends BTree[A] {
  override def rootOption: Some[A] = Some(data)

  override def isEmpty: Boolean = false
}

case object Empty extends BTree[Nothing] {
  override def rootOption: Option[Nothing] = None

  override def isEmpty: Boolean = true
}
