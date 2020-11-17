object Helper {
  def treeDepth[A](t: BTree[A]): Int = t match {
    case Empty => 0
    case Vertex(_, l, r) => 1 + (treeDepth(l) max treeDepth(r))
  }
}
