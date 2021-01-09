package Kalaha

import scala.annotation.tailrec

//Every node consist:
//actualBoard: actual state of board for appropriate move
//playerNumber: for actual move
//stonesDifference: difference of main bases' scores
//parent: for that node
//startMove: move at the beginning(from first level in tree), which give us expected score
//nextMove: if selected move gives the player the other extra one
class Node(var actualBoard: Board, var playerNumber: Int, var stonesDifference: Int, var parent: Node, var startMove: Int, var nextMove: Boolean) {

  var children: Array[Node] = Array[Node]()

  def addChild(node: Node): Node = {
    node.parent = this
    children = children.appended(node)
    node
  }

  def addChild(actualBoard: Board, playerNumber: Int, stonesDifference: Int, startMove: Int, nextMove: Boolean): Node = {
    val child: Node = new Node(actualBoard, playerNumber, stonesDifference, this, startMove, nextMove)
    children.appended(child)
    child
  }

  def removeChild(index: Int): Unit = {
    @tailrec
    def findIndex(array: Array[Node], counter: Int, resultArray: Array[Node]): Array[Node] = {
      if(counter == array.length) resultArray
      else findIndex(array, counter+1, if(counter == index) resultArray else resultArray.appended(array(counter)))
    }
    children = findIndex(children, 0, Array())
  }

  def isLeaf: Boolean = {
    children sameElements Array[Node]()
  }

  def getFirstChild(): Node = {
    if(children.length > 0) children.head
    else null
  }

  def getNextSibling(): Node = {
    if(parent == null) return null

    val copyChildren = parent.children
    val index = copyChildren.indexOf(this)
    if(copyChildren.length > index+1) copyChildren(index+1) else null
  }
}
