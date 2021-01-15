package DecisionTreePackage

import DecisionTreePackage.DecisionTree._
import GameboardPackage.Gameboard

import scala.annotation.tailrec

object DecisionTree {
  private val FIRST_INDEX_PLAYER1 = 0
  private val BASE_INDEX_PLAYER1 = 6
  private val FIRST_INDEX_PLAYER2 = 7
  private val BASE_INDEX_PLAYER2 = 13
  private val PLAYER_1_ROUND = 1
  private val COMPUTER_FORESEE_LEVELS = 2
}

class DecisionTree(var gameboard: Gameboard) {
  val root = new Node(gameboard.boardClone(),gameboard.getWhoseRound,0,-1)

  private class Node(private[this] val board: Gameboard, private[this] val playerNumber: Int,
                     private val level: Int, private val fieldChoice: Int) {

    private var parent: Node = _
    private val children: List[Node] = calculateChildren()
    private var advantage = if (board.getWhoseRound == playerNumber) board.calculateAdvantage()
                            else {
                              board.changePlayer()
                              val temp = board.calculateAdvantage()
                              board.changePlayer()
                              temp
                            }

    def calculateChildren(): List[Node] = { //This method is calculating best route for enemy and user at the same time
      if (board.endGameCheck()) return Nil
      def calculateChildrenHelp(currentIndex: Int): List[Node] = {
        if (currentIndex > BASE_INDEX_PLAYER1 || currentIndex > BASE_INDEX_PLAYER2) Nil
        else {
          val testBoard = board.boardClone()
          testBoard.playerMove(currentIndex)
          val child = new Node(testBoard.boardClone(),playerNumber,level+1,currentIndex)
          child.parent = this
          child.advantage = testBoard.calculateAdvantage()
          child :: calculateChildrenHelp(currentIndex + 1)
        }
      }
      calculateChildrenHelp(if (board.getWhoseRound == PLAYER_1_ROUND) FIRST_INDEX_PLAYER1 else FIRST_INDEX_PLAYER2)
    }

    def getChildren(): List[Node] = {
      children
    }

    def getAdvantage(): Int = {
      advantage
    }

    def getFieldChoice(): Int = {
      fieldChoice
    }

    def getParent(): Node = {
      parent
    }

    def getLevel(): Int = {
      level
    }
  }

  def createTree(): Unit = {
    @tailrec
    def createTreeHelp(nodeQueue: List[Node]): Unit = {
      nodeQueue match {
        case h :: t => if (h.getLevel() >= COMPUTER_FORESEE_LEVELS) ()
                       else {h.calculateChildren()
                            createTreeHelp(t ::: h.getChildren)}
      }
    }
    createTreeHelp(List(root))
  }

  private def findBestNode(): Node = {
    @tailrec
    def findBestNodeHelp(nodeQueue: List[Node], bestNode:Node): Node = {
       nodeQueue match {
         case h :: t  => if (h.getChildren == Nil && h.getAdvantage > bestNode.getAdvantage) findBestNodeHelp(t,h)
                         else if (h.getChildren == Nil) findBestNodeHelp(t,bestNode)
                         else findBestNodeHelp(t:::h.getChildren,bestNode)
         case Nil => bestNode
       }
    }
    findBestNodeHelp(List(root),root)
  }

  def findBestMove(): Int = {
    val bestMoveNode = findBestNode()
    var tempNode = bestMoveNode
    while (tempNode.getParent().getFieldChoice() != -1) {
      tempNode = tempNode.getParent()
    }
    tempNode.getFieldChoice()
  }
}
