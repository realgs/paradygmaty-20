package client

import server.GameState

object AI {

  private class Node(val children: Array[Node], val state: GameState, val idx: Int) {
    var points: (Int, Int) = state.points
    var nextBestMove: Int = -1

    def pointDiff(maximizingPlayer: String): Int = {
      if (maximizingPlayer == "S") points._1 - points._2
      else points._2 - points._1
    }

    def validChildren: Array[Node] = children.filter(node => node != null)

    def isLeaf: Boolean = children.forall(n => n == null)
  }

  def getNextMove(gameState: GameState, depth: Int): Int = {
    require(depth > 1)
    val root = buildTree(gameState, depth, -1)
    evaluateTree(root, gameState.nextTurn, alpha = Integer.MIN_VALUE, beta = Integer.MAX_VALUE)
    root.nextBestMove
  }

  private def buildTree(currState: GameState, depth: Int, idx: Int): Node = {
    depth match {
      case 1 => new Node(Array.fill(6)(null), currState, idx)
      case d =>
        val nodes: Array[Node] = Array.fill(6)(null)
        Range(0, 6).foreach(i => {
          if (currState.isMoveLegal(currState.nextTurn, i)) {
            val nextState = currState.play(currState.nextTurn, i)
            if (currState.nextTurn == nextState.nextTurn) {
              // Build additional node if this move would result in a player getting additional turn
              nodes(i) = buildTree(nextState, d, i)
            }
            else nodes(i) = buildTree(nextState, d - 1, i)
          }
        })
        new Node(nodes, currState, idx)
    }
  }

  private def evaluateTree(tree: Node, maximizingPlayer: String, alpha: Int, beta: Int): Unit = {
    if (!tree.isLeaf) {
      val nodes = tree.validChildren
      var bestMove: Node = null

      if (tree.state.nextTurn == maximizingPlayer) {
        var greatestDiff = Integer.MIN_VALUE
        var a = alpha
        var i = 0

        do {
          evaluateTree(nodes(i), maximizingPlayer, a, beta)
          val pointDiff = nodes(i).pointDiff(maximizingPlayer)
          if (pointDiff > greatestDiff) {
            greatestDiff = pointDiff
            bestMove = nodes(i)
          }
          a = math.max(a, greatestDiff)
          i += 1
        } while (beta > a && i < nodes.length)
      }
      else {
        var smallestDiff = Integer.MAX_VALUE
        var b = beta
        var i = 0

        do {
          evaluateTree(nodes(i), maximizingPlayer, alpha, b)
          val pointDiff = nodes(i).pointDiff(maximizingPlayer)
          if (pointDiff < smallestDiff) {
            smallestDiff = pointDiff
            bestMove = nodes(i)
          }
          b = math.min(b, smallestDiff)
          i += 1
        } while (b > alpha && i < nodes.length)
      }

      tree.points = bestMove.points
      tree.nextBestMove = bestMove.idx
    }
  }
}
