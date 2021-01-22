package engine

class BasicEngine extends KalahaEngine
{
  private var tree: DecisionTree = new DecisionTree

  override def calculateBestMove(depth: Int): Int =
    {
      calculate(depth, tree.root)
      val best_path: DecisionTree = findBestPath()
      tree = best_path
      best_path.root.value
    }

  def calculate(depth: Int, node: DecisionTree#Node): Unit =
    {

    }

  def findBestPath(): DecisionTree =
    {

    }
}

class DecisionTree
{
  class Node
  {
    var value: Int = 0
    var left: Node = _
    var right: Node = _

    def this(value: Int, left: Node, right: Node)
      {
        this()
        this.left = left
        this.right = right
      }
  }
  var root: Node = new Node

  def this(root: Node)
    {
      this()
      this.root = root
    }
}
