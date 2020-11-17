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

  def isLeaf: Boolean

  def getLeft: Option[BTree[A]]

  def getRight: Option[BTree[A]]

  def leftRoot: Option[A]

  def rightRoot: Option[A]

  def toBfsList: List[A]

}

object BTree {
  def apply[A](): BTree[A] = Empty

  def apply[A](data: A, left: BTree[A], right: BTree[A]): BTree[A] = Vertex(data, left, right)

  def apply[A](data: A): BTree[A] = Vertex(data)
}

case class Vertex[A](data: A, left: BTree[A], right: BTree[A]) extends BTree[A] {
  override def rootOption: Some[A] = Some(data)

  override def isEmpty: Boolean = false

  override def isLeaf: Boolean = left == Empty && right == Empty

  override def getLeft: Some[BTree[A]] = Some(left)

  override def getRight: Some[BTree[A]] = Some(right)

  override def leftRoot: Option[A] = getLeft.getOrElse(Empty).rootOption

  override def rightRoot: Option[A] = getRight.getOrElse(Empty).rootOption

  override def toBfsList: List[A] = {
    def auxBFS(queue: List[BTree[A]]): List[A] = {
      queue match {
        case Nil => Nil
        case Empty :: t => auxBFS(t)
        case Vertex(v, l, r) :: t => v :: auxBFS(t ::: List(l, r))
      }
    }

    auxBFS(List(this))
  }

}

object Vertex {
  def apply[A](data: A): Vertex[A] = Vertex(data, Empty, Empty)
}

case object Empty extends BTree[Nothing] {
  override def rootOption: Option[Nothing] = None

  override def isEmpty: Boolean = true

  override def isLeaf: Boolean = false

  override def getLeft: Option[BTree[Nothing]] = None

  override def getRight: Option[BTree[Nothing]] = None

  override def leftRoot: Option[Nothing] = None

  override def rightRoot: Option[Nothing] = None

  override def toBfsList: List[Nothing] = Nil
}
