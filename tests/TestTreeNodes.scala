// Adrian Chłopowiec
package tests

import algorithms.{Node, TreeNodes, TreeNodesPar}
import org.scalameter.measure

/*
  The third problem is calculating values in nodes below a certain node of a tree. It can also be used to calculate number
  of nodes below this node, if values are equal to 1. Parallel algorithm is much slower for small trees and once again starts
  giving faster results when the tree starts having many nodes.

  For example:
  d - depth of tree
  c - number of children of each node in tree

  d = 24
  c = 2
  Parallel time: 740.6855 ms
  Seq time: 1244.6514 ms

  d = 10
  c = 2
  Parallel time: 49.7229 ms
  Seq time: 3.3112 ms

  d = 14
  c = 3
  Parallel time: 219.2463 ms
  Seq time: 486.0361 ms
 */
object TestTreeNodes
{
  def main(args: Array[String]): Unit =
  {
    val tree = TreeNodes.generateTree(14, 3, 10)
    val seqTime = measure
    {
      TreeNodes.calculcateNodesValues(tree)
    }

    val parTime = measure
    {
      TreeNodesPar.calculateNodesValues(tree, 3)
    }

    println("TreeNodes")
    println("Parallel time: " + parTime)
    println("Seq time: " + seqTime)
  }
}
