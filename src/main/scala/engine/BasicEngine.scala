package engine

import Board.ImmutableBoard

class BasicEngine(private val player: Int, private[this] val depth: Int) extends KalahaEngine
{
  private var tree: DecisionTree = new DecisionTree
  private val other_player_id = (player + 1) % 2

  override def calculateBestMove(board: ImmutableBoard): Int =
  {
    if (depth <= 0)
      throw new IllegalArgumentException("Wrong depth")

    val validMoves = board.findValidMoves(player)
    calculate(depth, tree.root, board, player)
    val best_path: Int = findBestPath()
    val best_move: Int = validMoves(best_path)
    tree = new DecisionTree(tree.root.children(best_path))
    best_move
  }

  private def calculate(depth: Int, node: Node, board: ImmutableBoard, player_id: Int): Unit =
    {
      if(depth > 0)
        {
          val validMoves = board.findValidMoves(player_id)
          node.children = new Array(validMoves.length)
          var index: Int = 0
          for (i <- validMoves)
          {
            val (repeat, board_after_move) = board.makeMovesOnBoard(i, player_id)
            val player_advantage = board_after_move.getPoints(player) - board_after_move.getPoints(other_player_id)
            node.children(index) = new Node(player_advantage)
            if (repeat)
              calculate(depth - 1, node.children(index), board_after_move, player_id)
            else
              calculate(depth -1, node.children(index), board_after_move, (player_id + 1) % 2)

            index += 1
          }
        }
    }

  private def findBestPath(): Int =
    {
      var max: Int = 0
      var max_index: Int = 0
      for (i <- tree.root.children.indices)
        {
          val pathValue = search(tree.root.children(i), depth - 1, player)
          if (max < pathValue)
            {
              max = pathValue
              max_index = i
            }
        }
      max_index
    }

  private def search(node: Node, depth: Int, player_id: Int): Int =
  {
    if (depth == 0)
      node.value
    else
    {
      if(player_id == player)
        {
          var max: Int = 0
          for (i <- node.children.indices)
          {
            val pathValue = search(node.children(i), depth - 1, other_player_id)
            if (max < pathValue)
              max = pathValue
          }
          max
        }
      else
        {
          var min: Int = 200
          for (i <- node.children.indices)
            {
              val pathValue = search(node.children(i), depth - 1, player)
              if (min > pathValue)
                min = pathValue
            }
          min
        }
    }
  }

}

private class Node
{
  var value: Int = 0
  var children: Array[Node] = _

  def this(value: Int)
  {
    this()
    this.value = value
  }
}

private class DecisionTree
{

  var root: Node = new Node

  def this(root: Node)
    {
      this()
      this.root = root
    }
}
