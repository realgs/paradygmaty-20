package client

import server.GameState

object AI {
  private class Node(val children: Array[Node], val state: GameState)

  def getNextMove(gameState: GameState, depth: Int): Int = {
    require(depth > 1)
    val root = buildTree(gameState, depth)
    val (_, pit) = evaluateTree(root, gameState.nextTurn, -1)
    pit
  }

  private def buildTree(currState: GameState, depth: Int): Node = {
    depth match {
      case 1 => new Node(Array.fill(6)(null), currState)
      case d =>
        val nodes: Array[Node] = Array.fill(6)(null)
        Range(0, 6).foreach(i => {
          if (currState.isMoveLegal(currState.nextTurn, i)) {
            val nextState = currState.play(currState.nextTurn, i)
            if (currState.nextTurn == nextState.nextTurn) {
              // Build additional node if this move would result in a player getting additional turn
              nodes(i) = buildTree(nextState, d)
            }
            else nodes(i) = buildTree(nextState, d - 1)
          }
        })
        new Node(nodes, currState)
    }
  }

  private def evaluateTree(tree: Node, maxPlayer: String, move: Int): (Int, Int) = {
    val currentState = tree.state
    val nodes = tree.children

    if (nodes.forall(n => n == null)) {
      val (sPoints, nPoints) = currentState.points
      if (maxPlayer == "S") (sPoints - nPoints, move)
      else (nPoints - sPoints, move)
    }
    else {
      val evaluatedNodes = nodes
        .zip(Range(0, 6))
        .filter { case (node, _) => node != null }
        .map { case (node, i) =>
          val (points, _) = evaluateTree(node, maxPlayer, i)
          (points, i)
        }

      if (currentState.nextTurn == maxPlayer) evaluatedNodes.maxBy { case (points, _) => points }
      else evaluatedNodes.minBy { case (points, _) => points }
    }
  }
}
