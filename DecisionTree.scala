package Kalaha

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

class DecisionTree(var board: Board, var whoseMove: Int) {

  private val root: Node = new Node(board, -1, -1, null, 0, false)
  private var currentPlayer: Int = whoseMove
  private var currentOpponent: Int = if(currentPlayer == 1) 2 else 1

  def getRoot(): Node = {
    root
  }

  def changePlayers(): Unit = {
    val temp = currentPlayer
    currentPlayer = currentOpponent
    currentOpponent = temp
  }

  def createFirstLevel(): Unit = {
    for(x <- 0 to 5) {
      var holeNumber = x
      if(currentPlayer == 2) holeNumber += 7
      val boardAfterMove: Board = root.actualBoard.clone()
      if(boardAfterMove.getStonesNumber(holeNumber) != 0) {
        val nextMove = boardAfterMove.makeMove(holeNumber, currentPlayer)
        root.addChild(new Node(boardAfterMove, currentPlayer, boardAfterMove.getScore(currentPlayer) - boardAfterMove.getScore(currentOpponent), root, holeNumber, if(nextMove == 1) true else false))

      }
    }
  }

  def createNextLevel(node: Node): Unit = {
    var copiedNode = node
    val queue = new mutable.Queue[Node]()

    while(copiedNode != null) {
      queue.enqueue(copiedNode)
      copiedNode = copiedNode.getNextSibling()
    }
    copiedNode = node

    while(!queue.isEmpty) {
      copiedNode = queue.head
      if(!queue.head.nextMove) changePlayers()
      for(x <- 0 to 5) {
        var holeNumber = x
        if(currentPlayer == 2) holeNumber += 7
        if(copiedNode.actualBoard.getStonesNumber(holeNumber) != 0) {
          val boardAfterMove: Board = copiedNode.actualBoard.clone()
          val nextMove = boardAfterMove.makeMove(holeNumber, currentPlayer)
          copiedNode.addChild(new Node(boardAfterMove, currentPlayer, boardAfterMove.getScore(currentPlayer) - boardAfterMove.getScore(currentOpponent), copiedNode, holeNumber, if(nextMove == 1) true else false))
        }
      }
      if(!queue.head.nextMove) changePlayers()
      queue.dequeue()
    }
  }

  //Creating tree with required depth
  def createTree(): Unit = {
    createFirstLevel() //depth = 1
    def createTreeInner(node: Node): Unit = {
      var copiedNode = node
      createNextLevel(copiedNode)
      while(copiedNode.getNextSibling() != null) {
        copiedNode = copiedNode.getNextSibling()
        createNextLevel(copiedNode)
      }
    }
    createTreeInner(root.getFirstChild())
  }

  def printTreeByLevels(node: Node): Unit = {

    val queue = new mutable.Queue[Node]()
    queue.enqueue(node)

    while(!queue.isEmpty) {
      var nodesAlreadyPrinted = 0
      var beginningSize = queue.size
      for(x <- 0 to beginningSize-1) {
        nodesAlreadyPrinted += 1
        queue(x).actualBoard.printBoard
        for(y <- 0 to queue(x).children.length-1) {
          queue.enqueue(queue(x).children(y))
        }
      }
      for(x <- 0 to nodesAlreadyPrinted-1) {
          queue.dequeue()
      }
      if(!queue.isEmpty) {
        println("Next level: ")
      }
    }
  }

  //Visiting nodes in depth and return tuple of (difference of stones in main bases, move which we have to do)
  def createChoiceQueue(node: Node): mutable.Queue[(Int, Int, Boolean)] = {
    val queue = new mutable.Queue[(Int, Int, Boolean)]()
    queue.enqueue((node.stonesDifference, node.startMove, node.nextMove))
    var child: Node = node.getFirstChild()
    var childOfChild: Node = child.getFirstChild()

    while(child != null) {
      while(childOfChild != null) {
        queue.enqueue((childOfChild.stonesDifference, child.startMove, child.nextMove))
        childOfChild = childOfChild.getNextSibling()
      }
      child = child.getNextSibling()
      if(child != null) {
        childOfChild = child.getFirstChild()
      }
    }
    queue
  }

  //Choosing the biggest difference from decision tree due to queue with all nodes
  //If the element is equal to the current one, we have random number: if > 0.75 choose that number, otherwise choose current number
  //Returning hole number to choose by player
  def findBestDecision(results: mutable.Queue[(Int, Int, Boolean)]): Int = {
    @tailrec
    def findInner(results: mutable.Queue[(Int, Int, Boolean)], stonesDifference: Int, holeNumber: Int): Int = {
      if(results.isEmpty) holeNumber
      else if(results.head._3) results.head._2
      else if(results.head._1 > stonesDifference) findInner(results.tail, results.head._1, results.head._2)
      else if(results.head._1 == stonesDifference) {
        val r: Random = new Random()
        val number = r.nextDouble()
        if(number > 0.5) findInner(results.tail, results.head._1, results.head._2)
        else findInner(results.tail, stonesDifference, holeNumber)
      } else findInner(results.tail, stonesDifference, holeNumber)
    }
    findInner(results, -1, 0)
  }

}
