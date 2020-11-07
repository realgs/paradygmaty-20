package Lista4

import scala.annotation.tailrec

object Lista4Trees extends App{
  // Tree
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]


  // Exceptions
  class WrongValueOfArguments extends Exception
  class WrongTreesLevel extends Exception
  class InternalException extends Exception


  // Test Methods
  // used for testing exc 1
  def isTreeFull[A](tree: BT[A]): Boolean = {
    tree match {
      case Empty => true
      case Node(_, l, r) =>
        def innerIsTreeFull(leftTree: BT[A], rightTree: BT[A]): Boolean ={
          (leftTree, rightTree) match {
            case(Node(_, l1, r1), Node(_, l2, r2)) => innerIsTreeFull(l1, r1) && innerIsTreeFull(l2, r2)
            case(Empty, Empty) => true
            case(_, _) => false
          }
      }
        innerIsTreeFull(l, r)
    }
  }

  // used for testing exc 1
  def isTreeDepthRight[A](tree: BT[A], expectedLevel: Int): Boolean = {
    tree match {
      case Empty => false
      case Node(_, l, r) =>
        def innerIsTreeDepthRight(leftTree: BT[A], rightTree: BT[A], level: Int): Boolean ={
          (leftTree, rightTree) match {
            case(Empty, Empty) => expectedLevel == 0
            case(Node(_, l1, r1), Node(_, l2, r2)) => innerIsTreeDepthRight(l1, r1, level-1) == innerIsTreeDepthRight(l2, r2, level-1)
            case(_, _) => false
          }
        }
        innerIsTreeDepthRight(l, r, expectedLevel)
    }
  }

  // used for printing tree
  def inorder[A](bt:BT[A]):List[A] = bt match {
    case Node(v,l,r) => inorder(l) ::: v :: inorder(r)
    case Empty => Nil
  }

  // used in exc 2 and 3
  def chceckIfMatch[A](leftBT: BT[A], rightBT: BT[A]): Boolean = {
    if(isTreeFull(leftBT) && isTreeFull(rightBT)) {
      @tailrec
      def compareLevels(leftSubTree: BT[A], rightSubTree: BT[A]): Boolean =
        (leftSubTree, rightSubTree) match{
          case (Node(_, _, _), Empty) => false
          case (Empty, Node(_, _, _)) => false
          case (Empty, Empty) => true
          case (Node(_, left1, _), Node(_, left2, _)) => compareLevels(left1, left2) //skoro drzewa sa pelne to wystarczy ze sprawdze 1 sciezke
        }
      compareLevels(leftBT, rightBT)
    }else false
  }

  //////////////////////////////////////////////////zadania////////////////////////////////////////////////////////////////

  // zadanie 1 (3pkt)
  // dla amountOfLevels == 0 zwraca drzewo z samym korzeniem z wartoscia - potomkami korzenia sa Empty
  // dla amountOfLevels < 0 rzuca blad
  def createTree(amountOfLevels: Int, leftInterval: Int, rightInterval: Int): BT[Int] = {
    if (amountOfLevels < 0 || rightInterval < leftInterval) throw new WrongValueOfArguments
    val randomInt = scala.util.Random
    def innerCreateTree(levels: Int): BT[Int] = {
      if(levels == (-1)) Empty
      else Node(randomInt.nextInt(rightInterval-leftInterval+1)+leftInterval, innerCreateTree(levels-1), innerCreateTree(levels-1))
    }
    innerCreateTree(amountOfLevels)
  }


  // zadanie 2 (3pkt)
  def subtractTrees(leftTree: BT[Int], rightTree: BT[Int]):BT[Int] = {
    if (!chceckIfMatch(leftTree, rightTree)) throw new WrongTreesLevel
    else{
      (leftTree, rightTree) match{
        case (Node(value1, left1, right1), Node(value2, left2, right2)) => Node(value1 - value2, subtractTrees(left1, left2), subtractTrees(right1, right2))
        case (_, _) => Empty
      }
    }
  }

  /*val tree1 = createTree(3, 1, 50)
  println("First tree: " + inorder(tree1))
  val tree2 = createTree(3, 1, 50)
  println("Second tree: " + inorder(tree2))
  val resultTree12 = subtractTrees(tree1, tree2)
  println("Result tree: " + inorder(resultTree12))
  val tree3 = createTree(2, 1, 50)
  println("Third tree: " + inorder(tree3))*/
  //val failTree = subtractTrees(tree1, tree3) ==>> throw new WrongTreesLevel


  // zadanie 3 (wgłąb 1 pkt, wszerz 3 pkt)
  // w glab DFS
  def compareTreesDFS(leftTree: BT[Int], rightTree: BT[Int]): (BT[Int], BT[Int]) ={
    if (!chceckIfMatch(leftTree, rightTree)) throw new WrongTreesLevel
    else{
      def innerCompare(leftSubTree: BT[Int], rightSubTree: BT[Int]): (BT[Int], BT[Int]) =
        (leftSubTree, rightSubTree) match {
          case(Node(valL, Empty, Empty), Node(valR, Empty, Empty)) =>
            if(valL == valR) (Empty, Empty)
            else (Node(valL, Empty, Empty), Node(valR, Empty, Empty))

          case (Node(valL, left1, right1), Node(valR, left2, right2)) =>
            val resultLeftSubTrees = innerCompare(left1, left2)
            val resultRightSubTrees = innerCompare(right1, right2)
            if(valL != valR) (Node(valL, resultLeftSubTrees._1, resultRightSubTrees._1), Node(valR, resultLeftSubTrees._2, resultRightSubTrees._2))
            else{
              if(resultLeftSubTrees._1 == Empty && resultRightSubTrees._1 == Empty) (Empty, Empty)
              else (Node(-1, resultLeftSubTrees._1, resultRightSubTrees._1), Node(-1, resultLeftSubTrees._2, resultRightSubTrees._2))
            }
          case (_, _) =>   throw new InternalException
        }
      innerCompare(leftTree, rightTree)
    }
  }


  //wszerz BFS
  /*def compareTreesBFS(leftTree: BT[Int], rightTree: BT[Int]): (BT[Int], BT[Int]) ={
    if (!chceckIfMatch(leftTree, rightTree)) throw new WrongTreesLevel
    else{
      def innerCompare(leftSubTree: List[BT[Int]], rightSubTree:List[BT[Int]]): (BT[Int], BT[Int]) =
        (leftSubTree, rightSubTree) match {
          case (Node(valL, Empty, Empty)::_, Node(valR, Empty, Empty)::_) =>
            if(valL == valR) (Empty, Empty)
            else (Node(valL, Empty, Empty), Node(valR, Empty, Empty))

          case (Node(valL, left1, right1)::tailL, Node(valR, left2, right2)::tailR) =>
            val resultLeftSubTrees = innerCompare(tailL:::List(left1), tailR:::List(left2))
            val resultRightSubTrees = innerCompare(tailL:::List(right1), tailR:::List(right2))
            if(valL != valR) (Node(valL, resultLeftSubTrees._1, resultRightSubTrees._1), Node(valR, resultLeftSubTrees._2, resultRightSubTrees._2))
            else{
              if(resultLeftSubTrees._1 == Empty && resultRightSubTrees._1 == Empty) (Empty, Empty)
              else (Node(-1, resultLeftSubTrees._1, resultRightSubTrees._1), Node(-1, resultLeftSubTrees._2, resultRightSubTrees._2))
            }
          case (_, _) =>   throw new InternalException
        }
      innerCompare(List(leftTree), List(rightTree))
    }
  }*/

  def compareTreesBFS(leftTree: BT[Int], rightTree: BT[Int]): (BT[Int], BT[Int]) ={
    if (!chceckIfMatch(leftTree, rightTree)) throw new WrongTreesLevel
    else{
      def innerCompare(leftSubTree: BT[Int], rightSubTree:BT[Int]): (BT[Int], BT[Int]) =
        (leftSubTree, rightSubTree) match {
          case (Node(valL, Empty, Empty), Node(valR, Empty, Empty)) =>
            if(valL == valR) (Empty, Empty)
            else (Node(valL, Empty, Empty), Node(valR, Empty, Empty))

          case (Node(valL, left1, right1), Node(valR, left2, right2)) =>
            val resultLeftSubTrees = innerCompare(left1, left2)
            val resultRightSubTrees = innerCompare(right1, right2)
            if(valL != valR) (Node(valL, resultLeftSubTrees._1, resultRightSubTrees._1), Node(valR, resultLeftSubTrees._2, resultRightSubTrees._2))
            else{
              if(resultLeftSubTrees._1 == Empty && resultRightSubTrees._1 == Empty) (Empty, Empty)
              else (Node(-1, resultLeftSubTrees._1, resultRightSubTrees._1), Node(-1, resultLeftSubTrees._2, resultRightSubTrees._2))
            }
          case (_, _) =>   throw new InternalException
        }
      innerCompare(leftTree, rightTree)
    }
  }

}
