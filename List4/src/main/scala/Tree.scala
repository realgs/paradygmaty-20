import scala.annotation.tailrec
import scala.collection.immutable.Queue

sealed trait BTree[+A] {
  def rootOption: Option[A]

  def isEmpty: Boolean

  def isLeaf: Boolean

  def leftOption: Option[BTree[A]]

  def rightOption: Option[BTree[A]]

  def leftRoot: Option[A]

  def rightRoot: Option[A]

  def toReversedBfsList: List[A]

  def depth: Int

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

  override def leftOption: Option[BTree[A]] = if (isLeaf) None else Some(left)

  override def rightOption: Option[BTree[A]] = if (isLeaf) None else Some(right)

  override def leftRoot: Option[A] = leftOption.getOrElse(Empty).rootOption

  override def rightRoot: Option[A] = rightOption.getOrElse(Empty).rootOption

  override def toReversedBfsList: List[A] = {
    @tailrec
    def auxBFS(q: Queue[BTree[A]])(acc: List[A]): List[A] = {
      if (q.isEmpty) acc
      else {
        q.dequeue match {
          case (Empty, queue) => auxBFS(queue)(acc)
          case (Vertex(data, l, r), queue) => auxBFS(queue.enqueue(l).enqueue(r))(data :: acc)
        }
      }
    }

    auxBFS(Queue(this))(Nil)
  }

  override def depth: Int = {
    1 + left.depth max right.depth
  }

}

object Vertex {
  def apply[A](data: A): Vertex[A] = Vertex(data, Empty, Empty)
}

case object Empty extends BTree[Nothing] {
  override def rootOption: Option[Nothing] = None

  override def isEmpty: Boolean = true

  override def isLeaf: Boolean = false

  override def leftOption: Option[BTree[Nothing]] = None

  override def rightOption: Option[BTree[Nothing]] = None

  override def leftRoot: Option[Nothing] = None

  override def rightRoot: Option[Nothing] = None

  override def toReversedBfsList: List[Nothing] = Nil

  override def depth: Int = -1
}
