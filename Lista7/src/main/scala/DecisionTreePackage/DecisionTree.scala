package DecisionTreePackage

import DecisionTreePackage.DecisionTree._ //This import was made in order not to use the object namespace
import GameboardPackage.Gameboard

/* This tree makes bruteforce calculation for the best path in order to gain the biggest advantage based on
  calculated outputs. it is a tree that has x levels described in constant COMPUTER_FORESEE_LEVELS. Tree takes
  the best output at the lowest level of the tree and takes it as path to win.
 */

object DecisionTree {
  private val FIRST_INDEX_PLAYER1 = 0
  private val BASE_INDEX_PLAYER1 = 6
  private val FIRST_INDEX_PLAYER2 = 7
  private val BASE_INDEX_PLAYER2 = 13
  private val PLAYER_1_ROUND = 1
  private val PLAYER_2_ROUND = 2
  private val COMPUTER_FORESEE_LEVELS = 2
}

class DecisionTree(var gameboard: Gameboard) {
  private[this] val  root = new Node(gameboard.boardClone(),gameboard.getWhoseRound,0,null,-1)

  class Node(private[this] val board: Gameboard, private[this] val playerNumber: Int,
                     private val level: Int,private val parent: Node, private val fieldChoice: Int) {

    private var children: List[Node] = Nil
    private val advantage = if (board.getWhoseRound == playerNumber) board.calculateAdvantage()
                            else {
                              board.changePlayer()
                              val temp = board.calculateAdvantage()
                              board.changePlayer()
                              temp
                            }

    def calculateChildren(): List[Node] = { //This method is calculating best route for enemy and user at the same time
      def calculateChildrenHelp(currentIndex: Int): List[Node] = {
        if ((board.getWhoseRound == PLAYER_1_ROUND && (currentIndex >= BASE_INDEX_PLAYER1 || currentIndex < FIRST_INDEX_PLAYER1)
          || (board.getWhoseRound == PLAYER_2_ROUND && (currentIndex >= BASE_INDEX_PLAYER2 || currentIndex < FIRST_INDEX_PLAYER2)))) Nil
        else {
          if (!board.endGameCheck() && board.getElem(currentIndex) != 0) {
            val testBoard = board.boardClone()
            testBoard.playerMove(currentIndex)
            val child = new Node(testBoard, playerNumber, level + 1, this, currentIndex)
            child :: calculateChildrenHelp(currentIndex + 1)
          } else {
            if (currentIndex + 1 == BASE_INDEX_PLAYER1 || currentIndex + 1 >= BASE_INDEX_PLAYER2) Nil
            else calculateChildrenHelp(currentIndex + 1)
          }
        }
      }
      children = calculateChildrenHelp(if (board.getWhoseRound == PLAYER_1_ROUND) FIRST_INDEX_PLAYER1 else FIRST_INDEX_PLAYER2)
      children
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

    override def toString: String = s"[$advantage $level $fieldChoice]"
  }

  def createTree(): Unit = {
    def createTreeHelp(nodeQueue: List[Node]): Unit = {
      nodeQueue match {
        case h :: t => if (h.getLevel() >= COMPUTER_FORESEE_LEVELS) ()
                       else createTreeHelp(t ::: h.calculateChildren())
        case Nil => ()
      }
    }
    createTreeHelp(List(root))
  }

  private def findBestNode(): Node = {
    def findBestNodeHelp(nodeQueue: List[Node], bestNode:Node): Node = {
       nodeQueue match {
         case h :: t  => if (h.getChildren == Nil && h.getAdvantage > bestNode.getAdvantage) findBestNodeHelp(t,h)
                         else if (h.getChildren == Nil) findBestNodeHelp(t,bestNode)
                         else findBestNodeHelp(t:::h.getChildren,bestNode)
         case Nil => bestNode
       }
    }
    findBestNodeHelp(List(root),root.getChildren().head)
  }

  def findBestMove(): Int = {
    val bestMoveNode = findBestNode()
    var tempNode = bestMoveNode
    while (tempNode.getParent() != root) {
      tempNode = tempNode.getParent()
    }
    tempNode.getFieldChoice()
  }
}

